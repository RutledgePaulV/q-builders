package com.github.rutledgepaulv.visitors;


import com.github.rutledgepaulv.nodes.AbstractNode;
import com.github.rutledgepaulv.nodes.AndNode;
import com.github.rutledgepaulv.nodes.ComparisonNode;
import com.github.rutledgepaulv.nodes.OrNode;

public interface NodeVisitor<T> {

    T visit(AndNode node);

    T visit(OrNode node);

    T visit(ComparisonNode node);

    default T visit(AbstractNode node) {
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
