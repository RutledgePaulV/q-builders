package com.github.rutledgepaulv.qbuilders.delegates.concrete;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.delegates.virtual.NumberPropertyDelegate;
import com.github.rutledgepaulv.qbuilders.properties.concrete.LongProperty;
import com.github.rutledgepaulv.qbuilders.structures.FieldPath;

public final class LongPropertyDelegate<T extends QBuilder<T>> extends NumberPropertyDelegate<T, Long> implements LongProperty<T> {

    public LongPropertyDelegate(FieldPath field, T canonical) {
        super(field, canonical);
    }

}
