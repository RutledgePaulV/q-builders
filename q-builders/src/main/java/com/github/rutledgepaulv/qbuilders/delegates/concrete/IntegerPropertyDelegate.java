package com.github.rutledgepaulv.qbuilders.delegates.concrete;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.delegates.virtual.NumberPropertyDelegate;
import com.github.rutledgepaulv.qbuilders.properties.concrete.IntegerProperty;

public final class IntegerPropertyDelegate<T extends QBuilder<T>>
        extends NumberPropertyDelegate<T, Integer> implements IntegerProperty<T> {

    public IntegerPropertyDelegate(String field, T canonical) {
        super(field, canonical);
    }

}
