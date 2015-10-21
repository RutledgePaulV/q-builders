package com.github.rutledgepaulv.qbuilders.nodes;

import java.util.List;

public class OrNode extends LogicalNode {

    public OrNode(AbstractNode parent, List<AbstractNode> children) {
        super(parent, children);
    }

}
