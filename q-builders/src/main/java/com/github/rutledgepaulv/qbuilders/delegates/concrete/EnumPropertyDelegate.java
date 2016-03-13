package com.github.rutledgepaulv.qbuilders.delegates.concrete;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.delegates.virtual.ListablePropertyDelegate;
import com.github.rutledgepaulv.qbuilders.properties.concrete.EnumProperty;

public final class EnumPropertyDelegate<T extends QBuilder<T>, S extends Enum<S>>
        extends ListablePropertyDelegate<T,S> implements EnumProperty<T,S> {

    public EnumPropertyDelegate(String field, T canonical) {
        super(field, canonical);
    }

}
