/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.delegates.concrete.BooleanPropertyDelegate
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
import com.github.rutledgepaulv.qbuilders.delegates.virtual.ExistentialPropertyDelegate;
import com.github.rutledgepaulv.qbuilders.operators.ComparisonOperator;
import com.github.rutledgepaulv.qbuilders.properties.concrete.BooleanProperty;
import com.github.rutledgepaulv.qbuilders.structures.FieldPath;

import java.util.Collections;

public final class BooleanPropertyDelegate<T extends QBuilder<T>> extends ExistentialPropertyDelegate<T> implements BooleanProperty<T> {

    public BooleanPropertyDelegate(FieldPath field, T canonical) {
        super(field, canonical);
    }

    public final Condition<T> isTrue() {
        return condition(getField(), ComparisonOperator.EQ, Collections.singletonList(true));
    }

    public final Condition<T> isFalse() {
        return condition(getField(), ComparisonOperator.EQ, Collections.singletonList(false));
    }

}
