package com.github.rutledgepaulv.qbuilders.delegates.concrete;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.delegates.virtual.NumberPropertyDelegate;
import com.github.rutledgepaulv.qbuilders.properties.concrete.FloatProperty;
import com.github.rutledgepaulv.qbuilders.structures.FieldPath;

public final class FloatPropertyDelegate<T extends QBuilder<T>> extends NumberPropertyDelegate<T, Float> implements FloatProperty<T> {

    public FloatPropertyDelegate(FieldPath field, T canonical) {
        super(field, canonical);
    }

}
