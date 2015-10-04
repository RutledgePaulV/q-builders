package com.github.rutledgepaulv.nodes;

import com.github.rutledgepaulv.operators.ComparisonOperator;

import java.util.Collection;

public class ComparisonNode extends AbstractNode {

    private AbstractNode parent;
    private String field;
    private ComparisonOperator operator;
    private Collection<?> values;

    public ComparisonNode(AbstractNode parent, String field, ComparisonOperator operator, Collection<?> values) {
        this.operator = operator;
        this.field = field;
        this.parent = parent;
        this.values = values;
    }

    public AbstractNode getParent() {
        return parent;
    }

    public void setParent(AbstractNode parent) {
        this.parent = parent;
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
