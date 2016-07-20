/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.delegates.virtual.ListablePropertyDelegate
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
import com.github.rutledgepaulv.qbuilders.properties.virtual.ListableProperty;
import com.github.rutledgepaulv.qbuilders.structures.FieldPath;

import java.util.Collection;

import static java.util.Arrays.asList;

public abstract class ListablePropertyDelegate<T extends QBuilder<T>, S>
        extends EquitablePropertyDelegate<T, S> implements ListableProperty<T, S> {

    protected ListablePropertyDelegate(FieldPath field, T canonical) {
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
