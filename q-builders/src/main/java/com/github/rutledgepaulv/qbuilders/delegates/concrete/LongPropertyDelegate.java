package com.github.rutledgepaulv.qbuilders.delegates.concrete;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.delegates.virtual.NumberPropertyDelegate;
import com.github.rutledgepaulv.qbuilders.properties.concrete.LongProperty;

public final class LongPropertyDelegate<T extends QBuilder<T>> extends NumberPropertyDelegate<T, Long> implements LongProperty<T> {

    public LongPropertyDelegate(String field, T canonical) {
        super(field, canonical);
    }

}
