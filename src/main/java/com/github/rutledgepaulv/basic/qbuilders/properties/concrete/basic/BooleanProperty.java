package com.github.rutledgepaulv.basic.qbuilders.properties.concrete.basic;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.basic.qbuilders.properties.virtual.ExistentialProperty;

public interface BooleanProperty<T extends QBuilder<T>> extends ExistentialProperty<T> {

    Condition<T> isTrue();
    Condition<T> isFalse();

}
