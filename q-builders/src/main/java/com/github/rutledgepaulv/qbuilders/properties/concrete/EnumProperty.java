package com.github.rutledgepaulv.qbuilders.properties.concrete;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.properties.virtual.EquitableProperty;
import com.github.rutledgepaulv.qbuilders.properties.virtual.ListableProperty;

/**
 * A property view for fields with {@link Enum} values.
 *
 * @param <T> The type of the final builder.
 */
public interface EnumProperty<T extends QBuilder<T>, S extends Enum<S>>
        extends ListableProperty<T,S>, EquitableProperty<T, S> {}
