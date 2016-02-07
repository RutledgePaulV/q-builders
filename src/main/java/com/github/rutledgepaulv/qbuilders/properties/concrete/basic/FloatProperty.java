package com.github.rutledgepaulv.qbuilders.properties.concrete.basic;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.properties.virtual.NumberProperty;

/**
 * A property view for fields with {@link Float} values.
 *
 * @param <T> The type of the final builder.
 */
public interface FloatProperty<T extends QBuilder<T>> extends NumberProperty<T, Float> {}
