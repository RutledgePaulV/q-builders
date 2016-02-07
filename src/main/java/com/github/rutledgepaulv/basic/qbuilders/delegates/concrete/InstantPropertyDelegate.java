package com.github.rutledgepaulv.basic.qbuilders.delegates.concrete;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.delegates.virtual.InstantLikePropertyDelegate;
import com.github.rutledgepaulv.basic.qbuilders.properties.concrete.basic.InstantProperty;

import java.time.Instant;

public final class InstantPropertyDelegate<T extends QBuilder<T>>
        extends InstantLikePropertyDelegate<T, Instant> implements InstantProperty<T> {

    public InstantPropertyDelegate(String field, T canonical) {
        super(field, canonical);
    }

    @Override
    protected Instant normalize(Instant instant) {
        return instant;
    }

}
