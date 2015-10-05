package com.github.rutledgepaulv.qbuilders.nodes;

import com.github.rutledgepaulv.qbuilders.operators.LogicalOperator;

import java.util.List;

public abstract class LogicalNode extends AbstractNode {

    private LogicalOperator operator;

    public LogicalNode(AbstractNode parent, LogicalOperator operator) {
        super(parent);
        this.operator = operator;
    }

    public LogicalNode(AbstractNode parent, LogicalOperator operator, List<AbstractNode> children) {
        super(parent, children);
        this.operator = operator;
    }

    public LogicalOperator getOperator() {
        return operator;
    }

    public void setOperator(LogicalOperator operator) {
        this.operator = operator;
    }
}
