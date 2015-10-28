package com.github.rutledgepaulv.basic.qbuilders.delegates.virtual;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.basic.qbuilders.operators.basic.ComparisonOperator;
import com.github.rutledgepaulv.basic.qbuilders.properties.virtual.NumberProperty;

import java.util.Collections;

public abstract class NumberPropertyDelegate<T extends QBuilder<T>, S extends Number>
        extends ListablePropertyDelegate<T, S> implements NumberProperty<T, S> {

    protected NumberPropertyDelegate(String field, T canonical) {
        super(field, canonical);
    }

    public final Condition<T> gt(S number) {
        return condition(getField(), ComparisonOperator.GT, Collections.singletonList(number));
    }

    public final Condition<T> lt(S number) {
        return condition(getField(), ComparisonOperator.LT, Collections.singletonList(number));
    }

    public final Condition<T> gte(S number) {
        return condition(getField(), ComparisonOperator.GTE, Collections.singletonList(number));
    }

    public final Condition<T> lte(S number) {
        return condition(getField(), ComparisonOperator.LTE, Collections.singletonList(number));
    }

}
