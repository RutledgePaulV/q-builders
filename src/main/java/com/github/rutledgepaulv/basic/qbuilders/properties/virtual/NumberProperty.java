package com.github.rutledgepaulv.basic.qbuilders.properties.virtual;

import com.github.rutledgepaulv.basic.qbuilders.conditions.CompleteCondition;
import com.github.rutledgepaulv.basic.qbuilders.conditions.PartialCondition;

public interface NumberProperty<T extends PartialCondition, S extends Number>
        extends ListableProperty<T, S>, EquitableProperty<T, S> {

    CompleteCondition<T> gt(S number);

    CompleteCondition<T> lt(S number);

    CompleteCondition<T> gte(S number);

    CompleteCondition<T> lte(S number);

}
