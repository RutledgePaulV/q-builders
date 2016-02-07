package com.github.rutledgepaulv.basic.qbuilders.properties.concrete.basic;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.basic.qbuilders.properties.virtual.Property;


/**
 * A property view for multi-value fields containing objects who themselves
 * may have additional fields.
 *
 * @param <T> The type of the final builder.
 */
public interface ConditionProperty<T extends QBuilder<T>, S extends QBuilder<S>> extends Property<T> {

    /**
     * Mandates that any of the elements of the multi-valued fields must match the
     * provided condition exactly.
     *
     * @return The logically complete condition.
     */
    Condition<T> any(Condition<S> condition);

}
