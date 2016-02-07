package com.github.rutledgepaulv.qbuilders.delegates.virtual;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.qbuilders.operators.basic.ComparisonOperator;
import com.github.rutledgepaulv.qbuilders.properties.virtual.EquitableProperty;

import java.util.Collections;

public abstract class EquitablePropertyDelegate<T extends QBuilder<T>, S>
        extends ExistentialPropertyDelegate<T> implements EquitableProperty<T,S> {

    protected EquitablePropertyDelegate(String field, T canonical) {
        super(field, canonical);
    }

    public final Condition<T> eq(S value) {
        if(value == null) {
            return condition(getField(), ComparisonOperator.EX, Collections.singleton(false));
        }
        return condition(getField(), ComparisonOperator.EQ, Collections.singletonList(value));
    }

    public final Condition<T> ne(S value) {
        if(value == null) {
            return condition(getField(), ComparisonOperator.EX, Collections.singleton(true));
        }
        return condition(getField(), ComparisonOperator.NE, Collections.singletonList(value));
    }

}
