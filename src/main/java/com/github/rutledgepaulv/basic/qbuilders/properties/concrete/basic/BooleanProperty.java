package com.github.rutledgepaulv.basic.qbuilders.properties.concrete.basic;

import com.github.rutledgepaulv.basic.qbuilders.conditions.CompleteCondition;
import com.github.rutledgepaulv.basic.qbuilders.conditions.PartialCondition;
import com.github.rutledgepaulv.basic.qbuilders.properties.virtual.ExistentialProperty;

public interface BooleanProperty<T extends PartialCondition> extends ExistentialProperty<T> {

    CompleteCondition<T> isTrue();
    CompleteCondition<T> isFalse();

}
