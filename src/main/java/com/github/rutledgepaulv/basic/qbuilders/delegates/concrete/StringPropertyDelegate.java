package com.github.rutledgepaulv.basic.qbuilders.delegates.concrete;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.conditions.CompleteCondition;
import com.github.rutledgepaulv.basic.qbuilders.delegates.virtual.ListablePropertyDelegate;
import com.github.rutledgepaulv.basic.qbuilders.operators.basic.ComparisonOperator;
import com.github.rutledgepaulv.basic.qbuilders.properties.concrete.basic.StringProperty;

import java.util.Collections;

public class StringPropertyDelegate<T extends QBuilder<T>>
        extends ListablePropertyDelegate<T, String> implements StringProperty<T> {

    public StringPropertyDelegate(String field, T canonical) {
        super(field, canonical);
    }

    public final CompleteCondition<T> lexicallyAfter(String value) {
        return condition(getField(), ComparisonOperator.GT, Collections.singletonList(value));
    }

    public final CompleteCondition<T> lexicallyBefore(String value) {
        return condition(getField(), ComparisonOperator.LT, Collections.singletonList(value));
    }

}
