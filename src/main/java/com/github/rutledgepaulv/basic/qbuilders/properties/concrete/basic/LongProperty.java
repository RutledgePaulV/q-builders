package com.github.rutledgepaulv.basic.qbuilders.properties.concrete.basic;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.properties.virtual.NumberProperty;

/**
 * A property view for fields with {@link Long} values.
 *
 * @param <T> The type of the final builder.
 */
public interface LongProperty<T extends QBuilder<T>> extends NumberProperty<T, Long> {}
