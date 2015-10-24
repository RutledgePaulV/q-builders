package com.github.rutledgepaulv.basic.qbuilders.properties.virtual;

import com.github.rutledgepaulv.basic.qbuilders.conditions.CompleteCondition;
import com.github.rutledgepaulv.basic.qbuilders.conditions.PartialCondition;

public interface ExistentialProperty<T extends PartialCondition> extends Property<T> {

    CompleteCondition<T> exists();

    CompleteCondition<T> doesNotExist();

}
