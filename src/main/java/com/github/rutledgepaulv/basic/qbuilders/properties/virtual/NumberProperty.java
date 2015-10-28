package com.github.rutledgepaulv.basic.qbuilders.properties.virtual;

import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.basic.qbuilders.conditions.Partial;

public interface NumberProperty<T extends Partial<T>, S extends Number>
        extends ListableProperty<T, S>, EquitableProperty<T, S> {

    Condition<T> gt(S number);

    Condition<T> lt(S number);

    Condition<T> gte(S number);

    Condition<T> lte(S number);

}
