package com.github.rutledgepaulv.qbuilders.delegates.concrete;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.delegates.virtual.NumberPropertyDelegate;
import com.github.rutledgepaulv.qbuilders.properties.concrete.basic.ShortProperty;

public final class ShortPropertyDelegate<T extends QBuilder<T>>
        extends NumberPropertyDelegate<T, Short> implements ShortProperty<T> {

    public ShortPropertyDelegate(String field, T canonical) {
        super(field, canonical);
    }

}
