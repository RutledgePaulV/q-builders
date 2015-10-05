package com.github.rutledgepaulv.nodes;

import com.github.rutledgepaulv.operators.ComparisonOperator;

import java.util.Collection;

public class ComparisonNode extends AbstractNode {

    private String field;
    private ComparisonOperator operator;
    private Collection<?> values;

    public ComparisonNode(AbstractNode parent, String field, ComparisonOperator operator, Collection<?> values) {
        super(parent);
        this.operator = operator;
        this.field = field;
        this.values = values;
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
