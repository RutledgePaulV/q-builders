package com.github.rutledgepaulv.basic.qbuilders.properties.virtual;

import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.basic.qbuilders.conditions.Partial;

import java.util.Collection;

public interface ListableProperty<T extends Partial<T>, S> extends Property<T> {

    Condition<T> in(S... values);

    Condition<T> in(Collection<S> values);

    Condition<T> nin(S... values);

    Condition<T> nin(Collection<S> values);

}
