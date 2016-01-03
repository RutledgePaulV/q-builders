package com.github.rutledgepaulv.basic.qbuilders.properties.virtual;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;

public interface ExistentialProperty<T extends QBuilder<T>> extends Property<T> {

    Condition<T> exists();

    Condition<T> doesNotExist();

}
