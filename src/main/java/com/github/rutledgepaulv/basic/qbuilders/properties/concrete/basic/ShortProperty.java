package com.github.rutledgepaulv.basic.qbuilders.properties.concrete.basic;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.properties.virtual.NumberProperty;

/**
 * A property view for fields with {@link Short} values.
 *
 * @param <T> The type of the final builder.
 */
public interface ShortProperty<T extends QBuilder<T>> extends NumberProperty<T, Short> {}
