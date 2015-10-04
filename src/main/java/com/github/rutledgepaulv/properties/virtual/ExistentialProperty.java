package com.github.rutledgepaulv.properties.virtual;

import com.github.rutledgepaulv.conditions.CompleteCondition;
import com.github.rutledgepaulv.conditions.PartialCondition;

public interface ExistentialProperty<T extends PartialCondition> extends Property<T> {

    CompleteCondition<T> exists();

    CompleteCondition<T> doesNotExist();

}
