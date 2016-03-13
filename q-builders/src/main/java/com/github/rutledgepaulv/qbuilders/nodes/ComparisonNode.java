package com.github.rutledgepaulv.qbuilders.nodes;

import com.github.rutledgepaulv.qbuilders.operators.ComparisonOperator;

import java.util.Collection;

public final class ComparisonNode extends AbstractNode {

    private String field;
    private ComparisonOperator operator;
    private Collection<?> values;

    public ComparisonNode(LogicalNode parent) {
        super(parent);
    }

    public String getField() {
        return field;
    }

    public void setField(String field) {
        this.field = field;
    }

    public ComparisonOperator getOperator() {
        return operator;
    }

    public void setOperator(ComparisonOperator operator) {
        this.operator = operator;
    }

    public Collection<?> getValues() {
        return values;
    }

    public void setValues(Collection<?> values) {
        this.values = values;
    }
}
