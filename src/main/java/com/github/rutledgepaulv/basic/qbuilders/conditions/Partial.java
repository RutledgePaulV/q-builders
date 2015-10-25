package com.github.rutledgepaulv.basic.qbuilders.conditions;

import com.github.rutledgepaulv.basic.qbuilders.properties.concrete.basic.*;

import java.util.List;

@SuppressWarnings("unchecked")
public interface Partial<T extends Partial> {

    BooleanProperty<T> bool(String field);
    StringProperty<T> string(String field);
    LongProperty<T> longNum(String field);
    IntegerProperty<T> intNum(String field);
    ShortProperty<T> shortNum(String field);
    FloatProperty<T> floatNum(String field);
    DoubleProperty<T> doubleNum(String field);

    Condition<T> or(List<Condition<T>> conditions);
    Condition<T> and(List<Condition<T>> conditions);

    Condition<T> or(Condition<T> c1, Condition<T> c2, Condition<T>... cn);
    Condition<T> and(Condition<T> c1, Condition<T> c2, Condition<T>... cn);

}
