package com.github.rutledgepaulv.qbuilders.visitors;

import com.github.rutledgepaulv.qbuilders.nodes.AbstractNode;
import com.github.rutledgepaulv.qbuilders.nodes.AndNode;
import com.github.rutledgepaulv.qbuilders.nodes.ComparisonNode;
import com.github.rutledgepaulv.qbuilders.nodes.OrNode;
import com.github.rutledgepaulv.qbuilders.operators.ComparisonOperator;

import java.util.function.Function;
import java.util.stream.Collectors;

@SuppressWarnings("WeakerAccess")
public class RSQLVisitor extends NodeVisitor<String> {

    private final Function<Object, String> serializer;

    public RSQLVisitor() {
        this(DefaultSerializationStrategy.INSTANCE);
    }

    public RSQLVisitor(Function<Object, String> serializationStrategy) {
        this.serializer = serializationStrategy;
    }

    @Override
    protected String visit(AndNode node) {
        String body = node.getChildren().stream().map(this::visitAny).collect(Collectors.joining(";"));
        return nodeBelongsToParentExpression(node) ? "(" + body + ")" : body;
    }

    @Override
    protected String visit(OrNode node) {
        String body = node.getChildren().stream().map(this::visitAny).collect(Collectors.joining(","));
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
        } else if (ComparisonOperator.SUB_CONDITION_ANY.equals(operator)) {
            return node.getField() + "=q=" + serialize(condition(node));
        }

        throw new UnsupportedOperationException("This visitor does not support the operator " + operator + ".");
    }

    protected boolean nodeBelongsToParentExpression(AbstractNode node) {
        return node.getParent() != null;
    }

    protected String single(ComparisonNode node, String op) {
        return node.getField() + op + serialize(node.getValues().iterator().next());
    }

    protected String list(ComparisonNode node, String op) {
        return node.getField() + op + node.getValues().stream()
                .map(this::serialize).collect(Collectors.joining(",", "(", ")"));
    }

    protected String serialize(Object value) {
        return this.serializer.apply(value);
    }


    protected static class DefaultSerializationStrategy implements Function<Object ,String> {

        protected final static DefaultSerializationStrategy INSTANCE = new DefaultSerializationStrategy();

        private static final CharSequence DOUBLE_QUOTE = "\"";
        private static final CharSequence SINGLE_QUOTE = "\'";


        @Override
        public String apply(Object o) {
            String string = o.toString();
            if(string.contains("\\")) {
                string = string.replaceAll("\\\\","\\\\\\\\");
            }

            boolean containsDoubleQuotes = string.contains("\"");
            boolean containsSingleQuotes = string.contains("'");
            boolean containsBoth = containsDoubleQuotes && containsSingleQuotes;

            if (!(containsDoubleQuotes || containsSingleQuotes)) {
                return DOUBLE_QUOTE + string + DOUBLE_QUOTE;
            } else if(containsDoubleQuotes && !containsBoth) {
                return SINGLE_QUOTE + string + SINGLE_QUOTE;
            } else if (!containsBoth) {
                return DOUBLE_QUOTE + string + DOUBLE_QUOTE;
            } else {
                string = string.replaceAll("\"", "\\\\\"");
                return DOUBLE_QUOTE + string + DOUBLE_QUOTE;
            }
        }

    }


}
