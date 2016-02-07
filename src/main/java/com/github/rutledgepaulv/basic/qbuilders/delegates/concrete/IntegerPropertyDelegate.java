package com.github.rutledgepaulv.basic.qbuilders.delegates.concrete;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.delegates.virtual.NumberPropertyDelegate;
import com.github.rutledgepaulv.basic.qbuilders.properties.concrete.basic.IntegerProperty;

public final class IntegerPropertyDelegate<T extends QBuilder<T>>
        extends NumberPropertyDelegate<T, Integer> implements IntegerProperty<T> {

    public IntegerPropertyDelegate(String field, T canonical) {
        super(field, canonical);
    }

}
