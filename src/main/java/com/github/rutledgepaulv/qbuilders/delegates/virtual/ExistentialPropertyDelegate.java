package com.github.rutledgepaulv.qbuilders.delegates.virtual;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.qbuilders.operators.ComparisonOperator;
import com.github.rutledgepaulv.qbuilders.properties.virtual.ExistentialProperty;
import com.github.rutledgepaulv.qbuilders.structures.FieldPath;

import java.util.Collections;

public abstract class ExistentialPropertyDelegate<T extends QBuilder<T>> extends PropertyDelegate<T>
        implements ExistentialProperty<T> {

    protected ExistentialPropertyDelegate(FieldPath field, T canonical) {
        super(field, canonical);
    }

    public final Condition<T> exists() {
        return condition(getField(), ComparisonOperator.EX, Collections.singletonList(true));
    }

    public final Condition<T> doesNotExist() {
        return condition(getField(), ComparisonOperator.EX, Collections.singletonList(false));
    }

}
