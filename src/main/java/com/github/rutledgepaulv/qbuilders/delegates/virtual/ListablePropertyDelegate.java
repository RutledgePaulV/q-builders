package com.github.rutledgepaulv.qbuilders.delegates.virtual;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.qbuilders.operators.basic.ComparisonOperator;
import com.github.rutledgepaulv.qbuilders.properties.virtual.ListableProperty;

import java.util.Collection;

import static java.util.Arrays.asList;

public abstract class ListablePropertyDelegate<T extends QBuilder<T>, S>
        extends EquitablePropertyDelegate<T, S> implements ListableProperty<T, S> {

    protected ListablePropertyDelegate(String field, T canonical) {
        super(field, canonical);
    }

    @SafeVarargs
    public final Condition<T> in(S... values) {
        return condition(getField(), ComparisonOperator.IN, asList(values));
    }

    public final Condition<T> in(Collection<S> values) {
        return condition(getField(), ComparisonOperator.IN, values);
    }

    @SafeVarargs
    public final Condition<T> nin(S... values) {
        return condition(getField(), ComparisonOperator.NIN, asList(values));
    }

    public final Condition<T> nin(Collection<S> values) {
        return condition(getField(), ComparisonOperator.NIN, values);
    }

}
