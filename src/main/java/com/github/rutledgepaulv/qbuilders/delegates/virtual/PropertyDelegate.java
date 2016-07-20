/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.delegates.virtual.PropertyDelegate
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
import com.github.rutledgepaulv.qbuilders.properties.virtual.Property;
import com.github.rutledgepaulv.qbuilders.structures.FieldPath;

public abstract class PropertyDelegate<T extends QBuilder<T>> extends Delegate<T> implements Property<T> {

    private FieldPath field;

    protected PropertyDelegate(FieldPath field, T canonical) {
        super(canonical);
        this.field = field;
    }

    protected final FieldPath getField() {
        return field;
    }


}
