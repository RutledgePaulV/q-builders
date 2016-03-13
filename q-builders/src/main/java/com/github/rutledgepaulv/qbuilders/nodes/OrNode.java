package com.github.rutledgepaulv.qbuilders.nodes;

import java.util.List;

public final class OrNode extends LogicalNode {

    public OrNode() {}

    public OrNode(LogicalNode parent) {
        super(parent);
    }

    public OrNode(LogicalNode parent, List<AbstractNode> children) {
        super(parent, children);
    }


}
