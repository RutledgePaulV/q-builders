package com.github.rutledgepaulv.qbuilders.properties.concrete;

import com.github.rutledgepaulv.qbuilders.conditions.CompleteCondition;
import com.github.rutledgepaulv.qbuilders.conditions.PartialCondition;
import com.github.rutledgepaulv.qbuilders.properties.virtual.EquitableProperty;
import com.github.rutledgepaulv.qbuilders.properties.virtual.ListableProperty;


public interface StringProperty<T extends PartialCondition> extends EquitableProperty<T, String>, ListableProperty<T, String> {

    CompleteCondition<T> lexicallyAfter(String value);
    CompleteCondition<T> lexicallyBefore(String value);
}
