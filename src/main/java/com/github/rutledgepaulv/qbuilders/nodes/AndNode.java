package com.github.rutledgepaulv.qbuilders.nodes;

import java.util.List;

public final class AndNode extends LogicalNode {

    public AndNode() {}

    public AndNode(LogicalNode parent) {
        super(parent);
    }

    public AndNode(LogicalNode parent, List<AbstractNode> children) {
        super(parent, children);
    }

}
