package com.github.rutledgepaulv.basic.qbuilders.properties.concrete.basic;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.basic.qbuilders.properties.virtual.Property;

public interface ConditionProperty<T extends QBuilder<T>, S extends QBuilder<S>> extends Property<T> {

    Condition<T> any(Condition<S> condition);

}
