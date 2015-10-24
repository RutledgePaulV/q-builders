package com.github.rutledgepaulv.basic.qbuilders.delegates.virtual;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.conditions.CompleteCondition;
import com.github.rutledgepaulv.basic.qbuilders.operators.basic.ComparisonOperator;
import com.github.rutledgepaulv.basic.qbuilders.properties.virtual.ExistentialProperty;

import java.util.Collections;

public abstract class ExistentialPropertyDelegate<T extends QBuilder<T>> extends PropertyDelegate<T>
        implements ExistentialProperty<T> {

    protected ExistentialPropertyDelegate(String field, T canonical) {
        super(field, canonical);
    }

    public final CompleteCondition<T> exists() {
        return condition(getField(), ComparisonOperator.EX, Collections.singletonList(true));
    }

    public final CompleteCondition<T> doesNotExist() {
        return condition(getField(), ComparisonOperator.EX, Collections.singletonList(false));
    }

}