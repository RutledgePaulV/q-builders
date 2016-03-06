package com.github.rutledgepaulv.qbuilders.nodes;

import com.github.rutledgepaulv.qbuilders.visitors.NodeVisitor;

public abstract class AbstractNode implements Visitable {

    private LogicalNode parent;

    public AbstractNode() {}

    public AbstractNode(LogicalNode parent) {
        this.parent = parent;
    }

    public LogicalNode getParent() {
        return parent;
    }

    public void setParent(LogicalNode parent) {
        this.parent = parent;
    }

    @Override
    public <T> T visit(NodeVisitor<T> visitor) {
        return visitor.visitAny(this);
    }

}
