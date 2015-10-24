package com.github.rutledgepaulv.basic.qbuilders.properties.virtual;

import com.github.rutledgepaulv.basic.qbuilders.conditions.CompleteCondition;
import com.github.rutledgepaulv.basic.qbuilders.conditions.PartialCondition;

import java.util.Collection;

public interface ListableProperty<T extends PartialCondition, S> extends Property<T> {

    CompleteCondition<T> in(S... values);

    CompleteCondition<T> in(Collection<S> values);

    CompleteCondition<T> nin(S... values);

    CompleteCondition<T> nin(Collection<S> values);

}
