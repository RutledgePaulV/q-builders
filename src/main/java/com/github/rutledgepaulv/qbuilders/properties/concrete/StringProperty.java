/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.properties.concrete.StringProperty
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
import com.github.rutledgepaulv.qbuilders.properties.virtual.EquitableProperty;
import com.github.rutledgepaulv.qbuilders.properties.virtual.ListableProperty;

/**
 * A property view for fields with {@link String} values.
 *
 * @param <T> The type of the final builder.
 */
public interface StringProperty<T extends QBuilder<T>> extends EquitableProperty<T, String>, ListableProperty<T, String> {

    /**
     * Mandates that the value of the field must occur after the provided value
     * when sorted lexicographically.
     *
     * @param value The string that the value must occur after.
     * @return The logically complete condition.
     */
    Condition<T> lexicallyAfter(String value);

    /**
     * Mandates that the value of the field must occur before the provided value
     * when sorted lexicographically.
     *
     * @param value The string that the value must occur before.
     * @return The logically complete condition.
     */
    Condition<T> lexicallyBefore(String value);

    /**
     * Mandates that the value of the field must be equal to or occur before the provided value
     * when sorted lexicographically.
     *
     * @param value The string that the value must occur before or be equal to.
     * @return The logically complete condition.
     */
    Condition<T> lexicallyNotAfter(String value);

    /**
     * Mandates that the value of the field must be equal to or occur after the provided value
     * when sorted lexicographically.
     *
     * @param value The string that the value must occur after or be equal to.
     * @return The logically complete condition.
     */
    Condition<T> lexicallyNotBefore(String value);

    /**
     * Mandates that the value of the field must match the regular expression provided
     * in the form of a string pattern. The particular regex implementation is determined
     * by the backend visitor that is used. No normalization is done in order to ensure
     * that the regex string works across each backend the same way, so you'll need to take
     * care to use the right pattern against the right backend.
     *
     * @param pattern The regular expression to used, expressed as a string in the format expected
     *                by whichever backend visitor you plan to use to build the query.
     * @return The logically complete condition.
     */
    Condition<T> pattern(String pattern);
}
