package com.github.rutledgepaulv.basic.qbuilders.properties.virtual;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;

public interface EquitableProperty<T extends QBuilder<T>, S> extends ExistentialProperty<T> {

    Condition<T> eq(S value);

    Condition<T> ne(S value);

}
