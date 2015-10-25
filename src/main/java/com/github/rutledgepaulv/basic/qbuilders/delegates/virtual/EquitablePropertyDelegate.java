package com.github.rutledgepaulv.basic.qbuilders.delegates.virtual;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.basic.qbuilders.operators.basic.ComparisonOperator;
import com.github.rutledgepaulv.basic.qbuilders.properties.virtual.EquitableProperty;

import java.util.Collections;

public abstract class EquitablePropertyDelegate<T extends QBuilder<T>, S>
        extends ExistentialPropertyDelegate<T> implements EquitableProperty<T,S> {

    protected EquitablePropertyDelegate(String field, T canonical) {
        super(field, canonical);
    }

    public final Condition<T> eq(S value) {
        return condition(getField(), ComparisonOperator.EQ, Collections.singletonList(value));
    }

    public final Condition<T> ne(S value) {
        return condition(getField(), ComparisonOperator.NE, Collections.singletonList(value));
    }

}
