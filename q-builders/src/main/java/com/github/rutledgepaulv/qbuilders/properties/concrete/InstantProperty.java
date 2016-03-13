package com.github.rutledgepaulv.qbuilders.properties.concrete;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.properties.virtual.InstantLikeProperty;

import java.time.Instant;

/**
 * A property view for fields with {@link Instant} values.
 *
 * @param <T> The type of the final builder.
 */
public interface InstantProperty<T extends QBuilder<T>> extends InstantLikeProperty<T, Instant> {}
