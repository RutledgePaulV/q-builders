package com.github.rutledgepaulv.qbuilders.properties.virtual;

import com.github.rutledgepaulv.qbuilders.conditions.CompleteCondition;
import com.github.rutledgepaulv.qbuilders.conditions.PartialCondition;

public interface NumberProperty<T extends PartialCondition, S extends Number>
        extends Property<T>, ListableProperty<T, S>, EquitableProperty<T, S> {

    CompleteCondition<T> gt(S number);

    CompleteCondition<T> lt(S number);

    CompleteCondition<T> gte(S number);

    CompleteCondition<T> lte(S number);

}
