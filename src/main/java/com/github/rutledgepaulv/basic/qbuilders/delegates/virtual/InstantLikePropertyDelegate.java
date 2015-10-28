package com.github.rutledgepaulv.basic.qbuilders.delegates.virtual;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.basic.qbuilders.operators.basic.ComparisonOperator;
import com.github.rutledgepaulv.basic.qbuilders.properties.virtual.InstantLikeProperty;

import java.time.Instant;
import java.util.Collections;

@SuppressWarnings("unchecked")
public abstract class InstantLikePropertyDelegate<T extends QBuilder<T>, S>
        extends EquitablePropertyDelegate<T, S> implements InstantLikeProperty<T, S> {

    public InstantLikePropertyDelegate(String field, T canonical) {
        super(field, canonical);
    }

    @Override
    public final Condition<T> before(S dateTime, boolean exclusive) {
        return condition(getField(), exclusive ? ComparisonOperator.LT : ComparisonOperator.LTE,
                Collections.singletonList(normalize(dateTime)));
    }

    @Override
    public final Condition<T> after(S dateTime, boolean exclusive) {
        return condition(getField(), exclusive ? ComparisonOperator.GT : ComparisonOperator.GTE,
                Collections.singletonList(normalize(dateTime)));
    }

    @Override
    public final Condition<T> between(S after, boolean exclusiveAfter, S before, boolean exclusiveBefore) {
        Condition<T> afterCondition = new QBuilder().instant(getField()).after(after, exclusiveAfter);
        Condition<T> beforeCondition = new QBuilder().instant(getField()).before(before, exclusiveBefore);
        return and(afterCondition, beforeCondition);
    }

    protected abstract Instant normalize(S dateTime);

}
