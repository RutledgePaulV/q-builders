package com.github.rutledgepaulv.basic.qbuilders.properties.virtual;

import com.github.rutledgepaulv.basic.qbuilders.conditions.CompleteCondition;
import com.github.rutledgepaulv.basic.qbuilders.conditions.PartialCondition;

public interface EquitableProperty<T extends PartialCondition, S> extends ExistentialProperty<T> {

    CompleteCondition<T> eq(S value);

    CompleteCondition<T> ne(S value);

}
