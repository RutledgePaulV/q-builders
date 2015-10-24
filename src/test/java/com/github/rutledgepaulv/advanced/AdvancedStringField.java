package com.github.rutledgepaulv.advanced;

import com.github.rutledgepaulv.basic.qbuilders.conditions.CompleteCondition;
import com.github.rutledgepaulv.basic.qbuilders.conditions.PartialCondition;
import com.github.rutledgepaulv.basic.qbuilders.properties.concrete.basic.StringProperty;

/**
 * Provide an interface describing the advanced methods available
 */
public interface AdvancedStringField<T extends PartialCondition<T>> extends StringProperty<T> {

    CompleteCondition<T> regex(String pattern);

}