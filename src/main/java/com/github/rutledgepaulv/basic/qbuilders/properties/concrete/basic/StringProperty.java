package com.github.rutledgepaulv.basic.qbuilders.properties.concrete.basic;

import com.github.rutledgepaulv.basic.qbuilders.conditions.CompleteCondition;
import com.github.rutledgepaulv.basic.qbuilders.conditions.PartialCondition;
import com.github.rutledgepaulv.basic.qbuilders.properties.virtual.EquitableProperty;
import com.github.rutledgepaulv.basic.qbuilders.properties.virtual.ListableProperty;


public interface StringProperty<T extends PartialCondition> extends EquitableProperty<T, String>, ListableProperty<T, String> {

    CompleteCondition<T> lexicallyAfter(String value);
    CompleteCondition<T> lexicallyBefore(String value);
}
