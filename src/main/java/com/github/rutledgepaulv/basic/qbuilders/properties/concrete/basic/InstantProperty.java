package com.github.rutledgepaulv.basic.qbuilders.properties.concrete.basic;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.properties.virtual.InstantLikeProperty;

import java.time.Instant;

/**
 * A property view for fields with {@link Instant} values.
 *
 * @param <T> The type of the final builder.
 */
public interface InstantProperty<T extends QBuilder<T>> extends InstantLikeProperty<T, Instant> {}
