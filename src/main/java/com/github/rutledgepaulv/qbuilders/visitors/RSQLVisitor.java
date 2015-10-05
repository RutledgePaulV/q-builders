package com.github.rutledgepaulv.qbuilders.visitors;

import com.github.rutledgepaulv.qbuilders.nodes.*;

import java.util.stream.Collectors;

public class RSQLVisitor extends NodeVisitor<String> {

    @Override
    protected final String visit(AndNode node) {
        String body = node.getChildren().stream().map(this::visit).collect(Collectors.joining(";"));
        return nodeHasMultipleChildrenAndParent(node) ? "(" + body + ")" : body;
    }

    @Override
    protected final String visit(OrNode node) {
        String body = node.getChildren().stream().map(this::visit).collect(Collectors.joining(","));
        return nodeHasMultipleChildrenAndParent(node) ? "(" + body + ")" : body;
    }

    @Override
    protected final String visit(ComparisonNode node) {
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

        throw new UnsupportedOperationException("Unsupported operator.");
    }

    private boolean nodeHasMultipleChildrenAndParent(AbstractNode node) {
        return node.getChildren().size() > 1 && node.getParent() != null;
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
