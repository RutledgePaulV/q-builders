/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.delegates.virtual.InstantLikePropertyDelegate
 *  *  *
 *  *  * Copyright (C) 2016 Paul Rutledge <paul.v.rutledge@gmail.com>
 *  *  *
 *  *  * This software may be modified and distributed under the terms
 *  *  * of the MIT license.  See the LICENSE file for details.
 *  *
 *
 */

package com.github.rutledgepaulv.qbuilders.delegates.virtual;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.qbuilders.operators.ComparisonOperator;
import com.github.rutledgepaulv.qbuilders.properties.virtual.InstantLikeProperty;
import com.github.rutledgepaulv.qbuilders.structures.FieldPath;

import java.time.Instant;
import java.util.Collections;

@SuppressWarnings("unchecked")
public abstract class InstantLikePropertyDelegate<T extends QBuilder<T>, S>
        extends EquitablePropertyDelegate<T, S> implements InstantLikeProperty<T, S> {

    public InstantLikePropertyDelegate(FieldPath field, T canonical) {
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
        Condition<T> afterCondition = new QBuilder().instant(getField().asKey()).after(after, exclusiveAfter);
        Condition<T> beforeCondition = new QBuilder().instant(getField().asKey()).before(before, exclusiveBefore);
        return and(afterCondition, beforeCondition);
    }

    protected abstract Instant normalize(S dateTime);

}
