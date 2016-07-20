/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.delegates.concrete.StringPropertyDelegate
 *  *  *
 *  *  * Copyright (C) 2016 Paul Rutledge <paul.v.rutledge@gmail.com>
 *  *  *
 *  *  * This software may be modified and distributed under the terms
 *  *  * of the MIT license.  See the LICENSE file for details.
 *  *
 *
 */

package com.github.rutledgepaulv.qbuilders.delegates.concrete;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.qbuilders.delegates.virtual.ListablePropertyDelegate;
import com.github.rutledgepaulv.qbuilders.operators.ComparisonOperator;
import com.github.rutledgepaulv.qbuilders.properties.concrete.StringProperty;
import com.github.rutledgepaulv.qbuilders.structures.FieldPath;

import java.util.Collections;

public final class StringPropertyDelegate<T extends QBuilder<T>>
        extends ListablePropertyDelegate<T, String> implements StringProperty<T> {

    public StringPropertyDelegate(FieldPath field, T canonical) {
        super(field, canonical);
    }

    public final Condition<T> lexicallyAfter(String value) {
        return condition(getField(), ComparisonOperator.GT, Collections.singletonList(value));
    }

    public final Condition<T> lexicallyBefore(String value) {
        return condition(getField(), ComparisonOperator.LT, Collections.singletonList(value));
    }

    public final Condition<T> lexicallyNotAfter(String value) {
        return condition(getField(), ComparisonOperator.LTE, Collections.singletonList(value));
    }

    public final Condition<T> lexicallyNotBefore(String value) {
        return condition(getField(), ComparisonOperator.GTE, Collections.singletonList(value));
    }

    public Condition<T> pattern(String pattern) {
        return condition(getField(), ComparisonOperator.RE, Collections.singletonList(pattern));
    }

}
