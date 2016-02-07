package com.github.rutledgepaulv.testsupport.basic;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.qbuilders.properties.concrete.basic.*;

import static com.github.rutledgepaulv.testsupport.dirty.FieldUtil.getCurrentMethodName;

public class QModel extends QBuilder<QModel> {

    public static class QueryModelPredef {
        @SafeVarargs
        public static Condition<QModel> and(Condition<QModel> c1, Condition<QModel> c2, Condition<QModel>... cn) {
            return new QModel().and(c1, c2, cn);
        }

        @SafeVarargs
        public static Condition<QModel> or(Condition<QModel> c1, Condition<QModel> c2, Condition<QModel>... cn) {
            return new QModel().or(c1, c2, cn);
        }

        public static BooleanProperty<QModel> myBoolean() {
            return new QModel().myBoolean();
        }

        public static StringProperty<QModel> myString() {
            return new QModel().myString();
        }

        public static StringProperty<QModel> myString2() {
            return new QModel().myString2();
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

        public static StringProperty<QModel> myListOfStrings() {
            return new QModel().myListOfStrings();
        }

        public static InstantProperty<QModel> myDateTime() {
            return new QModel().myDateTime();
        }

        public static ConditionProperty<QModel, QModel> mySubList() {
            return new QModel().mySubList();
        }
    }

    private StringProperty<QModel> myString2() {
        return string(getCurrentMethodName());
    }

    public BooleanProperty<QModel> myBoolean() {
        return bool(getCurrentMethodName());
    }

    public StringProperty<QModel> myString() {
        return string(getCurrentMethodName());
    }

    public LongProperty<QModel> myLong() {
        return longNum(getCurrentMethodName());
    }

    public DoubleProperty<QModel> myDouble() {
        return doubleNum(getCurrentMethodName());
    }

    public IntegerProperty<QModel> myInteger() {
        return intNum(getCurrentMethodName());
    }

    public ShortProperty<QModel> myShort() {
        return shortNum(getCurrentMethodName());
    }

    public FloatProperty<QModel> myFloat() {
        return floatNum(getCurrentMethodName());
    }

    public StringProperty<QModel> myListOfStrings() {
        return string(getCurrentMethodName());
    }

    public InstantProperty<QModel> myDateTime() {
        return instant(getCurrentMethodName());
    }

    public ConditionProperty<QModel, QModel> mySubList() {
        return condition(getCurrentMethodName());
    }
}
