/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.delegates.concrete.IntegerPropertyDelegate
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
import com.github.rutledgepaulv.qbuilders.delegates.virtual.NumberPropertyDelegate;
import com.github.rutledgepaulv.qbuilders.properties.concrete.IntegerProperty;
import com.github.rutledgepaulv.qbuilders.structures.FieldPath;

public final class IntegerPropertyDelegate<T extends QBuilder<T>>
        extends NumberPropertyDelegate<T, Integer> implements IntegerProperty<T> {

    public IntegerPropertyDelegate(FieldPath field, T canonical) {
        super(field, canonical);
    }

}
