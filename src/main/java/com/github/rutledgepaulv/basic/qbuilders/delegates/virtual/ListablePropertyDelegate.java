package com.github.rutledgepaulv.basic.qbuilders.delegates.virtual;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.basic.qbuilders.operators.basic.ComparisonOperator;
import com.github.rutledgepaulv.basic.qbuilders.properties.virtual.ListableProperty;
import com.github.rutledgepaulv.basic.qbuilders.utilities.VarArgUtils;

import java.util.Collection;

public abstract class ListablePropertyDelegate<T extends QBuilder<T>, S>
        extends EquitablePropertyDelegate<T, S> implements ListableProperty<T, S> {

    protected ListablePropertyDelegate(String field, T canonical) {
        super(field, canonical);
    }

    @SafeVarargs
    public final Condition<T> in(S... values) {
        return condition(getField(), ComparisonOperator.IN, VarArgUtils.list(values));
    }

    public final Condition<T> in(Collection<S> values) {
        return condition(getField(), ComparisonOperator.IN, values);
    }

    @SafeVarargs
    public final Condition<T> nin(S... values) {
        return condition(getField(), ComparisonOperator.NIN, VarArgUtils.list(values));
    }

    public final Condition<T> nin(Collection<S> values) {
        return condition(getField(), ComparisonOperator.NIN, values);
    }

}
