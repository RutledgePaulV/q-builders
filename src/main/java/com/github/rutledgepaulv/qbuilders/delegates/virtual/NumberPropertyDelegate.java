/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.delegates.virtual.NumberPropertyDelegate
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
import com.github.rutledgepaulv.qbuilders.properties.virtual.NumberProperty;
import com.github.rutledgepaulv.qbuilders.structures.FieldPath;

import java.util.Collections;

public abstract class NumberPropertyDelegate<T extends QBuilder<T>, S extends Number>
        extends ListablePropertyDelegate<T, S> implements NumberProperty<T, S> {

    protected NumberPropertyDelegate(FieldPath field, T canonical) {
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
