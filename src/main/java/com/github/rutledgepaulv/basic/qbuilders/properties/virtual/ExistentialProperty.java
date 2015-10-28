package com.github.rutledgepaulv.basic.qbuilders.properties.virtual;

import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.basic.qbuilders.conditions.Partial;

public interface ExistentialProperty<T extends Partial<T>> extends Property<T> {

    Condition<T> exists();

    Condition<T> doesNotExist();

}
