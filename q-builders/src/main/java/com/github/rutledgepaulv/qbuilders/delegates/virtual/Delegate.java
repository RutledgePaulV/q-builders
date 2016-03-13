package com.github.rutledgepaulv.qbuilders.delegates.virtual;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;

public abstract class Delegate<T extends QBuilder<T>> extends QBuilder<T> {

    private T canonical;

    protected Delegate(T canonical) {
        this.canonical = canonical;
    }

    @Override
    protected final T self() {
        return canonical;
    }

}
