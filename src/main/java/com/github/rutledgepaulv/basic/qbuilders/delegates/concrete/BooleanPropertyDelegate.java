package com.github.rutledgepaulv.basic.qbuilders.delegates.concrete;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.basic.qbuilders.delegates.virtual.ExistentialPropertyDelegate;
import com.github.rutledgepaulv.basic.qbuilders.operators.basic.ComparisonOperator;
import com.github.rutledgepaulv.basic.qbuilders.properties.concrete.basic.BooleanProperty;

import java.util.Collections;

public class BooleanPropertyDelegate<T extends QBuilder<T>> extends ExistentialPropertyDelegate<T> implements BooleanProperty<T> {

    public BooleanPropertyDelegate(String field, T canonical) {
        super(field, canonical);
    }

    public final Condition<T> isTrue() {
        return condition(getField(), ComparisonOperator.EQ, Collections.singletonList(true));
    }

    public final Condition<T> isFalse() {
        return condition(getField(), ComparisonOperator.EQ, Collections.singletonList(false));
    }

}
