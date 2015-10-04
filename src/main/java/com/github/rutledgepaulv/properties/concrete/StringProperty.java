package com.github.rutledgepaulv.properties.concrete;

import com.github.rutledgepaulv.conditions.CompleteCondition;
import com.github.rutledgepaulv.conditions.PartialCondition;
import com.github.rutledgepaulv.properties.virtual.EquitableProperty;

public interface StringProperty<T extends PartialCondition> extends EquitableProperty<T, String> {

    CompleteCondition<T> lexicallyAfter(String value);
    CompleteCondition<T> lexicallyBefore(String value);
}
