package com.github.rutledgepaulv.basic.qbuilders.nodes;

import java.util.List;

public final class AndNode extends LogicalNode {

    public AndNode(AbstractNode parent, List<AbstractNode> children) {
        super(parent, children);
    }

}
