package com.github.rutledgepaulv.basic.qbuilders.properties.virtual;

import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.basic.qbuilders.conditions.Partial;

public interface InstantLikeProperty<T extends Partial<T>, S> extends EquitableProperty<T, S> {

    Condition<T> before(S dateTime, boolean exclusive);

    Condition<T> after(S dateTime, boolean exclusive);

    Condition<T> between(S after, boolean exclusiveAfter, S before, boolean exclusiveBefore);

}
