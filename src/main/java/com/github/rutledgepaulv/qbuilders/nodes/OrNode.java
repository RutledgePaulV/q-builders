package com.github.rutledgepaulv.qbuilders.nodes;

import com.github.rutledgepaulv.qbuilders.operators.LogicalOperator;

import java.util.List;

public class OrNode extends LogicalNode {

    public OrNode(AbstractNode parent, List<AbstractNode> children) {
        super(parent, LogicalOperator.OR, children);
    }

}
