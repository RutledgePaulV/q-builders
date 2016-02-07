package com.github.rutledgepaulv.basic.qbuilders.delegates.concrete;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.delegates.virtual.NumberPropertyDelegate;
import com.github.rutledgepaulv.basic.qbuilders.properties.concrete.basic.FloatProperty;

public final class FloatPropertyDelegate<T extends QBuilder<T>> extends NumberPropertyDelegate<T, Float> implements FloatProperty<T> {

    public FloatPropertyDelegate(String field, T canonical) {
        super(field, canonical);
    }

}
