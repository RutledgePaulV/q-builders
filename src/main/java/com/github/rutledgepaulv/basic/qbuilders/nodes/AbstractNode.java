package com.github.rutledgepaulv.basic.qbuilders.nodes;

import com.github.rutledgepaulv.basic.qbuilders.visitors.NodeVisitor;

import java.util.ArrayList;
import java.util.List;

public abstract class AbstractNode implements Visitable {

    private AbstractNode parent;
    private List<AbstractNode> children = new ArrayList<>();

    public AbstractNode(AbstractNode parent) {
        this.parent = parent;
    }

    public AbstractNode(AbstractNode parent, List<AbstractNode> children) {
        this.parent = parent;
        setChildren(children);
    }

    public List<AbstractNode> getChildren() {
        return children;
    }

    public void setChildren(List<AbstractNode> children) {
        this.children = children;
        children.forEach(child -> child.setParent(this));
    }


    public AbstractNode getParent() {
        return parent;
    }

    public void setParent(AbstractNode parent) {
        this.parent = parent;
    }

    @Override
    public <T> T visit(NodeVisitor<T> visitor) {
        return visitor.visit(this);
    }
}
