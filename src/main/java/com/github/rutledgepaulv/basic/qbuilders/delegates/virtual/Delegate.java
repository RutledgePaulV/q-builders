package com.github.rutledgepaulv.basic.qbuilders.delegates.virtual;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;

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
