package com.github.rutledgepaulv.basic.qbuilders.delegates.virtual;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.conditions.CompleteCondition;
import com.github.rutledgepaulv.basic.qbuilders.operators.basic.ComparisonOperator;
import com.github.rutledgepaulv.basic.qbuilders.properties.virtual.NumberProperty;

import java.util.Collections;

public abstract class NumberPropertyDelegate<T extends QBuilder<T>, S extends Number>
        extends ListablePropertyDelegate<T, S> implements NumberProperty<T, S> {

        protected NumberPropertyDelegate(String field, T canonical) {
            super(field, canonical);
        }

        public final CompleteCondition<T> gt(S number) {
            return condition(getField(), ComparisonOperator.GT, Collections.singletonList(number));
        }

        public final CompleteCondition<T> lt(S number) {
            return condition(getField(), ComparisonOperator.LT, Collections.singletonList(number));
        }

        public final CompleteCondition<T> gte(S number) {
            return condition(getField(), ComparisonOperator.GTE, Collections.singletonList(number));
        }

        public final CompleteCondition<T> lte(S number) {
            return condition(getField(), ComparisonOperator.LTE, Collections.singletonList(number));
        }

}
