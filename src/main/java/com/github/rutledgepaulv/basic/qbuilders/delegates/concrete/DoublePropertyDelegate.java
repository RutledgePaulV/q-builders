package com.github.rutledgepaulv.basic.qbuilders.delegates.concrete;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.delegates.virtual.NumberPropertyDelegate;
import com.github.rutledgepaulv.basic.qbuilders.properties.concrete.basic.DoubleProperty;

public final class DoublePropertyDelegate<T extends QBuilder<T>> extends NumberPropertyDelegate<T, Double> implements DoubleProperty<T> {

    public DoublePropertyDelegate(String field, T canonical) {
        super(field, canonical);
    }

}
