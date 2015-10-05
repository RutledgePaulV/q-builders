package com.github.rutledgepaulv.qbuilders.properties.concrete;

import com.github.rutledgepaulv.qbuilders.conditions.CompleteCondition;
import com.github.rutledgepaulv.qbuilders.conditions.PartialCondition;
import com.github.rutledgepaulv.qbuilders.properties.virtual.ExistentialProperty;

public interface BooleanProperty<T extends PartialCondition> extends ExistentialProperty<T> {

    CompleteCondition<T> isTrue();
    CompleteCondition<T> isFalse();

}
