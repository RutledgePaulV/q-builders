package com.github.rutledgepaulv.qbuilders.nodes;

import com.github.rutledgepaulv.qbuilders.operators.LogicalOperator;

import java.util.List;

public class AndNode extends LogicalNode {

    public AndNode(AbstractNode parent, List<AbstractNode> children) {
        super(parent, LogicalOperator.AND, children);
    }

}
