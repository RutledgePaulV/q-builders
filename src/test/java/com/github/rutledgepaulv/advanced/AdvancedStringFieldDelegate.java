package com.github.rutledgepaulv.advanced;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.basic.qbuilders.delegates.concrete.StringPropertyDelegate;
import com.github.rutledgepaulv.basic.qbuilders.operators.advanced.AdvancedMongoOperator;

import java.util.Collections;

/**
 * Create an implementation to back it
 */
public class AdvancedStringFieldDelegate<T extends QBuilder<T>> extends StringPropertyDelegate<T>
        implements AdvancedStringField<T> {

    public AdvancedStringFieldDelegate(String field, T canonical) {
        super(field, canonical);
    }

    @Override
    public Condition<T> regex(String pattern) {
        return condition(getField(), AdvancedMongoOperator.REGEX, Collections.singletonList(pattern));
    }

}
