package com.github.rutledgepaulv.qbuilders.nodes;

import com.github.rutledgepaulv.qbuilders.operators.ComparisonOperator;
import com.github.rutledgepaulv.qbuilders.structures.FieldPath;

import java.util.Collection;

public final class ComparisonNode extends AbstractNode {

    private FieldPath path;
    private ComparisonOperator operator;
    private Collection<?> values;

    public ComparisonNode(LogicalNode parent) {
        super(parent);
    }

    public FieldPath getField() {
        return path;
    }

    public void setField(FieldPath field) {
        this.path = field;
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
