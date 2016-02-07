package com.github.rutledgepaulv.basic.qbuilders.properties.virtual;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;

/**
 * For properties that may or may not exist.
 *
 * @param <T> The final type of the builder.
 */
public interface ExistentialProperty<T extends QBuilder<T>> extends Property<T> {

    /**
     * Specifies that the selected field must exist (and be non-null).
     *
     * @return The logically complete condition
     */
    Condition<T> exists();

    /**
     * Specifies that the selected field must not exist (or be equal to null).
     *
     * @return The logically complete condition
     */
    Condition<T> doesNotExist();

}
