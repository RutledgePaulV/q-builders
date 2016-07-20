/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.properties.virtual.EquitableProperty
 *  *  *
 *  *  * Copyright (C) 2016 Paul Rutledge <paul.v.rutledge@gmail.com>
 *  *  *
 *  *  * This software may be modified and distributed under the terms
 *  *  * of the MIT license.  See the LICENSE file for details.
 *  *
 *
 */

package com.github.rutledgepaulv.qbuilders.properties.virtual;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.conditions.Condition;

/**
 * For properties that may or may not be equal.
 *
 * @param <T> The final type of the builder.
 */
public interface EquitableProperty<T extends QBuilder<T>, S> extends ExistentialProperty<T> {

    /**
     * Specifies that the value of the field must be equal to the provided value.
     * @param value The value that the field must be equal to.
     * @return The logically complete condition.
     */
    Condition<T> eq(S value);

    /**
     * Specifies that the value of the field must not be equal to the provided value.
     * @param value The value that the field must not be equal to.
     * @return The logically complete condition.
     */
    Condition<T> ne(S value);

}
