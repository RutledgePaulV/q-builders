package com.github.rutledgepaulv.qbuilders.visitors;


import com.github.rutledgepaulv.qbuilders.nodes.*;

public abstract class NodeVisitor<T> {

    protected abstract T visit(AndNode node);

    protected abstract T visit(OrNode node);

    protected abstract T visit(ComparisonNode node);

    public final T visit(AbstractNode node) {

        // skip straight to the children if it's a logical node with one member
        if(node instanceof LogicalNode && node.getChildren().size() == 1) {
            return visit(node.getChildren().get(0));
        }

        if(node instanceof AndNode){
            return visit((AndNode)node);
        } else if (node instanceof OrNode){
            return visit((OrNode)node);
        } else if (node instanceof ComparisonNode) {
            return visit((ComparisonNode)node);
        }

        return null;
    }

}
