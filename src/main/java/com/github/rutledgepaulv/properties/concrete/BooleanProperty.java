package com.github.rutledgepaulv.properties.concrete;

import com.github.rutledgepaulv.conditions.CompleteCondition;
import com.github.rutledgepaulv.conditions.PartialCondition;
import com.github.rutledgepaulv.properties.virtual.ExistentialProperty;

public interface BooleanProperty<T extends PartialCondition> extends ExistentialProperty<T> {

    CompleteCondition<T> isTrue();
    CompleteCondition<T> isFalse();

}
