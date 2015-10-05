package com.github.rutledgepaulv.qbuilders.properties.virtual;

import com.github.rutledgepaulv.qbuilders.conditions.CompleteCondition;
import com.github.rutledgepaulv.qbuilders.conditions.PartialCondition;

public interface EquitableProperty<T extends PartialCondition, S> extends ExistentialProperty<T> {

    CompleteCondition<T> eq(S value);

    CompleteCondition<T> ne(S value);

}
