package com.github.rutledgepaulv.basic.qbuilders.properties.concrete.basic;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.basic.qbuilders.properties.virtual.EquitableProperty;
import com.github.rutledgepaulv.basic.qbuilders.properties.virtual.ListableProperty;


public interface StringProperty<T extends QBuilder<T>> extends EquitableProperty<T, String>, ListableProperty<T, String> {

    Condition<T> lexicallyAfter(String value);
    Condition<T> lexicallyBefore(String value);
    Condition<T> lexicallyNotAfter(String value);
    Condition<T> lexicallyNotBefore(String value);

}
