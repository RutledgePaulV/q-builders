package com.github.rutledgepaulv.visitors;

import com.github.rutledgepaulv.nodes.AndNode;
import com.github.rutledgepaulv.nodes.ComparisonNode;
import com.github.rutledgepaulv.nodes.OrNode;

import java.util.stream.Collectors;

public class RSQLVisitor implements NodeVisitor<String> {

    @Override
    public final String visit(AndNode node) {
        return node.getChildren().stream().map(this::visit)
                .collect(Collectors.joining(";"));
    }

    @Override
    public final String visit(OrNode node) {
        return node.getChildren().stream().map(this::visit)
                .collect(Collectors.joining(","));
    }

    @Override
    public final String visit(ComparisonNode node) {
        switch (node.getOperator()) {
            case EQ:
                return single(node, "==");
            case NE:
                return single(node, "!=");
            case EX:
                return single(node, "=ex=");
            case GT:
                return single(node, "=gt=");
            case GTE:
                return single(node, "=ge=");
            case LT:
                return single(node, "=lt=");
            case LTE:
                return single(node, "=le=");
            case IN:
                return list(node, "=in=");
            case NIN:
                return list(node, "=nin=");
        }
        return null;
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
