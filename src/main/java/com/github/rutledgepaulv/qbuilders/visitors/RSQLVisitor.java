/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.visitors.RSQLVisitor
 *  *  *
 *  *  * Copyright (C) 2016 Paul Rutledge <paul.v.rutledge@gmail.com>
 *  *  *
 *  *  * This software may be modified and distributed under the terms
 *  *  * of the MIT license.  See the LICENSE file for details.
 *  *
 *
 */

package com.github.rutledgepaulv.qbuilders.visitors;

import com.github.rutledgepaulv.qbuilders.nodes.AbstractNode;
import com.github.rutledgepaulv.qbuilders.nodes.AndNode;
import com.github.rutledgepaulv.qbuilders.nodes.ComparisonNode;
import com.github.rutledgepaulv.qbuilders.nodes.OrNode;
import com.github.rutledgepaulv.qbuilders.operators.ComparisonOperator;

import java.util.Objects;
import java.util.function.Function;

import static java.util.stream.Collectors.joining;

@SuppressWarnings("WeakerAccess")
public class RSQLVisitor extends AbstractVoidContextNodeVisitor<String> {

    private final Function<Object, String> serializer;

    public RSQLVisitor() {
        this(DefaultSerializationStrategy.INSTANCE);
    }

    public RSQLVisitor(Function<Object, String> serializationStrategy) {
        this.serializer = serializationStrategy;
    }

    @Override
    protected String visit(AndNode node) {
        String body = node.getChildren().stream().map(this::visitAny).collect(joining(";"));
        return nodeBelongsToParentExpression(node) ? "(" + body + ")" : body;
    }

    @Override
    protected String visit(OrNode node) {
        String body = node.getChildren().stream().map(this::visitAny).collect(joining(","));
        return nodeBelongsToParentExpression(node) ? "(" + body + ")" : body;
    }

    @Override
    protected String visit(ComparisonNode node) {

        ComparisonOperator operator = node.getOperator();

        if(ComparisonOperator.EQ.equals(operator)) {
            return single(node, "==");
        } else if(ComparisonOperator.NE.equals(operator)) {
            return single(node, "!=");
        } else if (ComparisonOperator.EX.equals(operator)) {
            return single(node, "=ex=");
        } else if (ComparisonOperator.GT.equals(operator)) {
            return single(node, "=gt=");
        } else if (ComparisonOperator.LT.equals(operator)) {
            return single(node, "=lt=");
        } else if (ComparisonOperator.GTE.equals(operator)) {
            return single(node, "=ge=");
        } else if (ComparisonOperator.LTE.equals(operator)) {
            return single(node, "=le=");
        } else if (ComparisonOperator.IN.equals(operator)) {
            return list(node, "=in=");
        } else if (ComparisonOperator.NIN.equals(operator)) {
            return list(node, "=out=");
        } else if (ComparisonOperator.RE.equals(operator)) {
            return single(node, "=re=");
        } else if (ComparisonOperator.SUB_CONDITION_ANY.equals(operator)) {
            return node.getField().asKey() + "=q=" + serialize(condition(node));
        }

        throw new UnsupportedOperationException("This visitor does not support the operator " + operator + ".");
    }

    protected boolean nodeBelongsToParentExpression(AbstractNode node) {
        return node.getParent() != null;
    }

    protected String single(ComparisonNode node, String op) {
        return node.getField().asKey() + op + serialize(single(node.getValues()));
    }

    protected String list(ComparisonNode node, String op) {
        return node.getField().asKey() + op + node.getValues().stream()
                .map(this::serialize).collect(joining(",", "(", ")"));
    }

    protected String serialize(Object value) {
        return this.serializer.apply(value);
    }


    protected static class DefaultSerializationStrategy implements Function<Object ,String> {

        protected final static DefaultSerializationStrategy INSTANCE = new DefaultSerializationStrategy();

        private static final CharSequence DOUBLE_QUOTE = "\"";
        private static final CharSequence SINGLE_QUOTE = "\'";


        @Override
        public String apply(Object value) {

            String string = Objects.toString(value);

            if(string.contains("\\")) {
                string = string.replaceAll("\\\\","\\\\\\\\");
            }

            boolean containsDoubleQuotes = string.contains("\"");
            boolean containsSingleQuotes = string.contains("'");

            if(!containsDoubleQuotes) {
                return DOUBLE_QUOTE + string + DOUBLE_QUOTE;
            } else if (!containsSingleQuotes) {
                return SINGLE_QUOTE + string + SINGLE_QUOTE;
            } else {
                string = string.replaceAll("\"", "\\\\\"");
                return DOUBLE_QUOTE + string + DOUBLE_QUOTE;
            }

        }

    }


}
