package com.github.rutledgepaulv.qbuilders.delegates.concrete;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.delegates.virtual.InstantLikePropertyDelegate;
import com.github.rutledgepaulv.qbuilders.properties.concrete.InstantProperty;
import com.github.rutledgepaulv.qbuilders.structures.FieldPath;

import java.time.Instant;

public final class InstantPropertyDelegate<T extends QBuilder<T>>
        extends InstantLikePropertyDelegate<T, Instant> implements InstantProperty<T> {

    public InstantPropertyDelegate(FieldPath field, T canonical) {
        super(field, canonical);
    }

    @Override
    protected Instant normalize(Instant instant) {
        return instant;
    }

}
