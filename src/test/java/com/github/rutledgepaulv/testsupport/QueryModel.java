/*
 *
 *  *  com.github.rutledgepaulv.testsupport.QueryModel
 *  *  *
 *  *  * Copyright (C) 2016 Paul Rutledge <paul.v.rutledge@gmail.com>
 *  *  *
 *  *  * This software may be modified and distributed under the terms
 *  *  * of the MIT license.  See the LICENSE file for details.
 *  *
 *
 */

package com.github.rutledgepaulv.testsupport;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.qbuilders.properties.concrete.*;

import static com.github.rutledgepaulv.testsupport.FieldUtil.getCurrentMethodName;

public class QueryModel extends QBuilder<QueryModel> {

    public static class QueryModelPredef {
        @SafeVarargs
        public static Condition<QueryModel> and(Condition<QueryModel> c1, Condition<QueryModel> c2, Condition<QueryModel>... cn) {
            return new QueryModel().and(c1, c2, cn);
        }

        @SafeVarargs
        public static Condition<QueryModel> or(Condition<QueryModel> c1, Condition<QueryModel> c2, Condition<QueryModel>... cn) {
            return new QueryModel().or(c1, c2, cn);
        }

        public static BooleanProperty<QueryModel> myBoolean() {
            return new QueryModel().myBoolean();
        }

        public static StringProperty<QueryModel> myString() {
            return new QueryModel().myString();
        }

        public static StringProperty<QueryModel> myString2() {
            return new QueryModel().myString2();
        }
        public static LongProperty<QueryModel> myLong() {
            return new QueryModel().myLong();
        }

        public static DoubleProperty<QueryModel> myDouble() {
            return new QueryModel().myDouble();
        }

        public static IntegerProperty<QueryModel> myInteger() {
            return new QueryModel().myInteger();
        }

        public static ShortProperty<QueryModel> myShort() {
            return new QueryModel().myShort();
        }

        public static FloatProperty<QueryModel> myFloat() {
            return new QueryModel().myFloat();
        }

        public static StringProperty<QueryModel> myListOfStrings() {
            return new QueryModel().myListOfStrings();
        }

        public static InstantProperty<QueryModel> myDateTime() {
            return new QueryModel().myDateTime();
        }

        public static ConditionProperty<QueryModel, QueryModel> mySubList() {
            return new QueryModel().mySubList();
        }

        public static EnumProperty<QueryModel, DomainModel.MyEnum> myEnum() {
            return new QueryModel().myEnum();
        }
    }

    public EnumProperty<QueryModel, DomainModel.MyEnum> myEnum() {
        return enumeration(getCurrentMethodName());
    }

    private StringProperty<QueryModel> myString2() {
        return string(getCurrentMethodName());
    }

    public BooleanProperty<QueryModel> myBoolean() {
        return bool(getCurrentMethodName());
    }

    public StringProperty<QueryModel> myString() {
        return string(getCurrentMethodName());
    }

    public LongProperty<QueryModel> myLong() {
        return longNum(getCurrentMethodName());
    }

    public DoubleProperty<QueryModel> myDouble() {
        return doubleNum(getCurrentMethodName());
    }

    public IntegerProperty<QueryModel> myInteger() {
        return intNum(getCurrentMethodName());
    }

    public ShortProperty<QueryModel> myShort() {
        return shortNum(getCurrentMethodName());
    }

    public FloatProperty<QueryModel> myFloat() {
        return floatNum(getCurrentMethodName());
    }

    public StringProperty<QueryModel> myListOfStrings() {
        return string(getCurrentMethodName());
    }

    public InstantProperty<QueryModel> myDateTime() {
        return instant(getCurrentMethodName());
    }

    public ConditionProperty<QueryModel, QueryModel> mySubList() {
        return condition(getCurrentMethodName());
    }
}
