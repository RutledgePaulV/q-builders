package com.github.rutledgepaulv.nodes;

import com.github.rutledgepaulv.operators.LogicalOperator;

import java.util.List;

public class AndNode extends LogicalNode {

    public AndNode(AbstractNode parent, List<AbstractNode> children) {
        super(parent, LogicalOperator.AND, children);
    }

}
