package com.github.rutledgepaulv.qbuilders.conditions;

import com.github.rutledgepaulv.qbuilders.properties.concrete.*;

import java.util.List;

@SuppressWarnings("unchecked")
public interface PartialCondition<T extends PartialCondition> {

    BooleanProperty<T> bool(String field);
    StringProperty<T> string(String field);
    LongProperty<T> longNum(String field);
    IntegerProperty<T> intNum(String field);
    ShortProperty<T> shortNum(String field);
    FloatProperty<T> floatNum(String field);
    DoubleProperty<T> doubleNum(String field);

    CompleteCondition<T> or(List<CompleteCondition<T>> conditions);
    CompleteCondition<T> and(List<CompleteCondition<T>> conditions);

    CompleteCondition<T> or(CompleteCondition<T> c1, CompleteCondition<T> c2, CompleteCondition<T>... cn);
    CompleteCondition<T> and(CompleteCondition<T> c1, CompleteCondition<T> c2, CompleteCondition<T>... cn);

}
