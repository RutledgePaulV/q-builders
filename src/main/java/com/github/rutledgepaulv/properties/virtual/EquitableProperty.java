package com.github.rutledgepaulv.properties.virtual;

import com.github.rutledgepaulv.conditions.CompleteCondition;
import com.github.rutledgepaulv.conditions.PartialCondition;

public interface EquitableProperty<T extends PartialCondition, S> extends ExistentialProperty<T> {

    CompleteCondition<T> eq(S value);

    CompleteCondition<T> ne(S value);

}
