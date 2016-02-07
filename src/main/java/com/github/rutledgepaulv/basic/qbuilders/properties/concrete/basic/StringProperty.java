package com.github.rutledgepaulv.basic.qbuilders.properties.concrete.basic;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.basic.qbuilders.properties.virtual.EquitableProperty;
import com.github.rutledgepaulv.basic.qbuilders.properties.virtual.ListableProperty;

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

}
