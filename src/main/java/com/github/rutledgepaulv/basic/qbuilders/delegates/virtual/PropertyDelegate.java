package com.github.rutledgepaulv.basic.qbuilders.delegates.virtual;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.properties.virtual.Property;

public abstract class PropertyDelegate<T extends QBuilder<T>> extends Delegate<T> implements Property<T> {

    private String field;

    protected PropertyDelegate(String field, T canonical) {
        super(canonical);
        this.field = field;
    }

    protected final String getField() {
        return field;
    }


}
