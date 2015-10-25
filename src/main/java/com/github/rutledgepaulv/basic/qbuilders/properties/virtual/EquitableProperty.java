package com.github.rutledgepaulv.basic.qbuilders.properties.virtual;

import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.basic.qbuilders.conditions.Partial;

public interface EquitableProperty<T extends Partial, S> extends ExistentialProperty<T> {

    Condition<T> eq(S value);

    Condition<T> ne(S value);

}
