package com.github.rutledgepaulv.qbuilders.nodes;

import java.util.List;

public class AndNode extends LogicalNode {

    public AndNode(AbstractNode parent, List<AbstractNode> children) {
        super(parent, children);
    }

}
