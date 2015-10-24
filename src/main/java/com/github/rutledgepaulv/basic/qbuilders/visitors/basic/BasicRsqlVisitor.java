package com.github.rutledgepaulv.basic.qbuilders.visitors.basic;

import com.github.rutledgepaulv.basic.qbuilders.nodes.AbstractNode;
import com.github.rutledgepaulv.basic.qbuilders.nodes.AndNode;
import com.github.rutledgepaulv.basic.qbuilders.nodes.ComparisonNode;
import com.github.rutledgepaulv.basic.qbuilders.nodes.OrNode;
import com.github.rutledgepaulv.basic.qbuilders.operators.basic.ComparisonOperator;
import com.github.rutledgepaulv.basic.qbuilders.visitors.NodeVisitor;

import java.util.stream.Collectors;

public class BasicRsqlVisitor extends NodeVisitor<String> {

    @Override
    protected final String visit(AndNode node) {
        String body = node.getChildren().stream().map(this::visit).collect(Collectors.joining(";"));
        return nodeBelongsToParentExpression(node) ? "(" + body + ")" : body;
    }

    @Override
    protected final String visit(OrNode node) {
        String body = node.getChildren().stream().map(this::visit).collect(Collectors.joining(","));
        return nodeBelongsToParentExpression(node) ? "(" + body + ")" : body;
    }

    @Override
    protected final String visit(ComparisonNode node) {

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
        }

        return null;
    }

    private boolean nodeBelongsToParentExpression(AbstractNode node) {
        return node.getParent() != null;
    }

    private String single(ComparisonNode node, String op) {
        return node.getField() + op + serialize(node.getValues().iterator().next());
    }

    private String list(ComparisonNode node, String op) {
        return node.getField() + op + node.getValues().stream()
                .map(this::serialize).collect(Collectors.joining(",", "(", ")"));
    }

    protected String serialize(Object value) {
        return value.toString();
    }

}
