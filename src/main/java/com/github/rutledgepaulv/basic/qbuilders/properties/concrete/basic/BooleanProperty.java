package com.github.rutledgepaulv.basic.qbuilders.properties.concrete.basic;

import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.basic.qbuilders.conditions.Partial;
import com.github.rutledgepaulv.basic.qbuilders.properties.virtual.ExistentialProperty;

public interface BooleanProperty<T extends Partial<T>> extends ExistentialProperty<T> {

    Condition<T> isTrue();
    Condition<T> isFalse();

}
