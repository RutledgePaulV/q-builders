package com.github.rutledgepaulv.basic.qbuilders.properties.virtual;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;

public interface InstantLikeProperty<T extends QBuilder<T>, S> extends EquitableProperty<T, S> {

    Condition<T> before(S dateTime, boolean exclusive);

    Condition<T> after(S dateTime, boolean exclusive);

    Condition<T> between(S after, boolean exclusiveAfter, S before, boolean exclusiveBefore);

}
