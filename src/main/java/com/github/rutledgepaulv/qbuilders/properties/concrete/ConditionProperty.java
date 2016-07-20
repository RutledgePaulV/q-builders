/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.properties.concrete.ConditionProperty
 *  *  *
 *  *  * Copyright (C) 2016 Paul Rutledge <paul.v.rutledge@gmail.com>
 *  *  *
 *  *  * This software may be modified and distributed under the terms
 *  *  * of the MIT license.  See the LICENSE file for details.
 *  *
 *
 */

package com.github.rutledgepaulv.qbuilders.properties.concrete;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.qbuilders.properties.virtual.Property;


/**
 * A property view for multi-value fields containing objects who themselves
 * may have additional fields.
 *
 * @param <T> The type of the final builder.
 */
public interface ConditionProperty<T extends QBuilder<T>, S extends QBuilder<S>> extends Property<T> {

    /**
     * Mandates that at least one of the elements of the multi-valued fields must match the
     * provided condition exactly.
     *
     * @param condition The condition that should be imposed individually against each element
     *                  in the multi valued field.
     *
     * @return The logically complete condition.
     */
    Condition<T> any(Condition<S> condition);

}
