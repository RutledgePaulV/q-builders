package com.github.rutledgepaulv.qbuilders.properties.virtual;

import com.github.rutledgepaulv.qbuilders.conditions.CompleteCondition;
import com.github.rutledgepaulv.qbuilders.conditions.PartialCondition;

public interface ExistentialProperty<T extends PartialCondition> extends Property<T> {

    CompleteCondition<T> exists();

    CompleteCondition<T> doesNotExist();

}
