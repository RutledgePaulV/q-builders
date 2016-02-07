package com.github.rutledgepaulv.qbuilders.delegates.concrete;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.qbuilders.delegates.virtual.PropertyDelegate;
import com.github.rutledgepaulv.qbuilders.operators.basic.ComparisonOperator;
import com.github.rutledgepaulv.qbuilders.properties.concrete.basic.ConditionProperty;

import java.util.Collections;

public final class ConditionPropertyDelegate<T extends QBuilder<T>, S extends QBuilder<S>>
        extends PropertyDelegate<T> implements ConditionProperty<T, S> {

    public ConditionPropertyDelegate(String field, T canonical) {
        super(field, canonical);
    }

    @Override
    public Condition<T> any(Condition<S> condition) {
        return condition(getField(), ComparisonOperator.SUB_CONDITION_ANY, Collections.singleton(condition));
    }

}
