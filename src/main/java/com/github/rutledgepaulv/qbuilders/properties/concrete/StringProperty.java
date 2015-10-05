package com.github.rutledgepaulv.qbuilders.properties.concrete;

import com.github.rutledgepaulv.qbuilders.conditions.CompleteCondition;
import com.github.rutledgepaulv.qbuilders.conditions.PartialCondition;
import com.github.rutledgepaulv.qbuilders.properties.virtual.EquitableProperty;

public interface StringProperty<T extends PartialCondition> extends EquitableProperty<T, String> {

    CompleteCondition<T> lexicallyAfter(String value);
    CompleteCondition<T> lexicallyBefore(String value);
}
