package com.github.rutledgepaulv.conditions;

import com.github.rutledgepaulv.properties.concrete.*;

import java.util.List;

public interface PartialCondition<T extends PartialCondition> {

    BooleanProperty<T> booleanField(String field);
    StringProperty<T> stringField(String field);
    ShortProperty<T> shortField(String field);
    IntegerProperty<T> integerField(String field);
    LongProperty<T> longField(String field);
    FloatProperty<T> floatField(String field);
    DoubleProperty<T> doubleField(String field);
    CompleteCondition<T> and(CompleteCondition<T> c1, CompleteCondition<T> c2, CompleteCondition<T>... cn);
    CompleteCondition<T> or(CompleteCondition<T> c1, CompleteCondition<T> c2, CompleteCondition<T>... cn);
    CompleteCondition<T> and(List<CompleteCondition<T>> conditions);
    CompleteCondition<T> or(List<CompleteCondition<T>> conditions);

}
