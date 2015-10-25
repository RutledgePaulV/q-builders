package com.github.rutledgepaulv.basic.qbuilders.properties.concrete.basic;

import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.basic.qbuilders.conditions.Partial;
import com.github.rutledgepaulv.basic.qbuilders.properties.virtual.EquitableProperty;
import com.github.rutledgepaulv.basic.qbuilders.properties.virtual.ListableProperty;


public interface StringProperty<T extends Partial> extends EquitableProperty<T, String>, ListableProperty<T, String> {

    Condition<T> lexicallyAfter(String value);
    Condition<T> lexicallyBefore(String value);
}
