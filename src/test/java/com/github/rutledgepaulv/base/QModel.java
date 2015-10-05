package com.github.rutledgepaulv.base;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.conditions.CompleteCondition;
import com.github.rutledgepaulv.qbuilders.properties.concrete.*;

import static com.github.rutledgepaulv.dirty.FieldUtil.getCurrentMethodName;

public class QModel extends QBuilder<QModel> {

    public static class QueryModelPredef {
        @SafeVarargs
        public static CompleteCondition<QModel> and(CompleteCondition<QModel> c1, CompleteCondition<QModel> c2, CompleteCondition<QModel>... cn) {
            return new QModel().and(c1, c2, cn);
        }
        @SafeVarargs
        public static CompleteCondition<QModel> or(CompleteCondition<QModel> c1, CompleteCondition<QModel> c2, CompleteCondition<QModel>... cn) {
            return new QModel().or(c1, c2, cn);
        }
        public static BooleanProperty<QModel> myBoolean() {
            return new QModel().myBoolean();
        }
        public static StringProperty<QModel> myString(){
            return new QModel().myString();
        }
        public static LongProperty<QModel> myLong() {
            return new QModel().myLong();
        }
        public static DoubleProperty<QModel> myDouble() {
            return new QModel().myDouble();
        }
        public static IntegerProperty<QModel> myInteger() {
            return new QModel().myInteger();
        }
        public static ShortProperty<QModel> myShort() {
            return new QModel().myShort();
        }
        public static FloatProperty<QModel> myFloat() {
            return new QModel().myFloat();
        }
    }

    public BooleanProperty<QModel> myBoolean() {
        return booleanField(getCurrentMethodName());
    }

    public StringProperty<QModel> myString() {
        return stringField(getCurrentMethodName());
    }

    public LongProperty<QModel> myLong() {
        return longField(getCurrentMethodName());
    }

    public DoubleProperty<QModel> myDouble() {
        return doubleField(getCurrentMethodName());
    }

    public IntegerProperty<QModel> myInteger() {
        return integerField(getCurrentMethodName());
    }

    public ShortProperty<QModel> myShort() {
        return shortField(getCurrentMethodName());
    }

    public FloatProperty<QModel> myFloat() {
        return floatField(getCurrentMethodName());
    }

}
