package com.github.rutledgepaulv.qbuilders.delegates.virtual;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.properties.virtual.Property;
import com.github.rutledgepaulv.qbuilders.structures.FieldPath;

public abstract class PropertyDelegate<T extends QBuilder<T>> extends Delegate<T> implements Property<T> {

    private FieldPath field;

    protected PropertyDelegate(FieldPath field, T canonical) {
        super(canonical);
        this.field = field;
    }

    protected final FieldPath getField() {
        return field;
    }


}
