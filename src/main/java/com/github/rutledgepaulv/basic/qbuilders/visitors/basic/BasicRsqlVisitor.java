package com.github.rutledgepaulv.basic.qbuilders.visitors.basic;

import com.github.rutledgepaulv.basic.qbuilders.nodes.AbstractNode;
import com.github.rutledgepaulv.basic.qbuilders.nodes.AndNode;
import com.github.rutledgepaulv.basic.qbuilders.nodes.ComparisonNode;
import com.github.rutledgepaulv.basic.qbuilders.nodes.OrNode;
import com.github.rutledgepaulv.basic.qbuilders.operators.basic.ComparisonOperator;
import com.github.rutledgepaulv.basic.qbuilders.visitors.NodeVisitor;

import java.util.stream.Collectors;

public class BasicRsqlVisitor extends NodeVisitor<String> {

    private static final CharSequence DOUBLE_QUOTE = "\"";
    private static final CharSequence SINGLE_QUOTE = "\'";

    @Override
    protected final String visit(AndNode node) {
        String body = node.getChildren().stream().map(this::visitAny).collect(Collectors.joining(";"));
        return nodeBelongsToParentExpression(node) ? "(" + body + ")" : body;
    }

    @Override
    protected final String visit(OrNode node) {
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
            return list(node, "=nin=");
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
        String rawValue = value.toString();
        if (!rawValue.contains(DOUBLE_QUOTE)) {
            return DOUBLE_QUOTE + rawValue + DOUBLE_QUOTE;
        } else if (!rawValue.contains(SINGLE_QUOTE)) {
            return SINGLE_QUOTE + rawValue + SINGLE_QUOTE;
        } else {
            throw new IllegalArgumentException("Your query values cannot contain both a single quote and a double quote.");
        }
    }

}
