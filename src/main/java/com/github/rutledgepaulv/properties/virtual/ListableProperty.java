package com.github.rutledgepaulv.properties.virtual;

import com.github.rutledgepaulv.conditions.CompleteCondition;
import com.github.rutledgepaulv.conditions.PartialCondition;

import java.util.Collection;

public interface ListableProperty<T extends PartialCondition, S> extends Property<T> {

    CompleteCondition<T> in(S... values);

    CompleteCondition<T> in(Collection<S> values);

    CompleteCondition<T> nin(S... values);

    CompleteCondition<T> nin(Collection<S> values);

}
