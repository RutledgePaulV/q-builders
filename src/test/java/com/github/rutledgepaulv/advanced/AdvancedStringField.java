package com.github.rutledgepaulv.advanced;

import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.basic.qbuilders.conditions.Partial;
import com.github.rutledgepaulv.basic.qbuilders.properties.concrete.basic.StringProperty;

/**
 * Provide an interface describing the advanced methods available
 */
public interface AdvancedStringField<T extends Partial<T>> extends StringProperty<T> {

    Condition<T> regex(String pattern);

}