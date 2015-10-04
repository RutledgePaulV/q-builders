package com.github.rutledgepaulv.nodes;

import com.github.rutledgepaulv.operators.LogicalOperator;

import java.util.List;

public class OrNode extends LogicalNode {

    public OrNode(AbstractNode parent, List<AbstractNode> children) {
        super(parent, LogicalOperator.OR, children);
    }

}
