package com.github.rutledgepaulv.base;

import com.github.rutledgepaulv.builders.QueryBuilder;
import com.github.rutledgepaulv.conditions.CompleteCondition;
import com.github.rutledgepaulv.properties.concrete.*;

import static com.github.rutledgepaulv.dirty.FieldUtil.getCurrentMethodName;

public class QueryModel extends QueryBuilder<QueryModel> {

    public static class QueryModelPredef {
        @SafeVarargs
        public static CompleteCondition<QueryModel> and(CompleteCondition<QueryModel> c1, CompleteCondition<QueryModel> c2, CompleteCondition<QueryModel>... cn) {
            return new QueryModel().and(c1, c2, cn);
        }
        @SafeVarargs
        public static CompleteCondition<QueryModel> or(CompleteCondition<QueryModel> c1, CompleteCondition<QueryModel> c2, CompleteCondition<QueryModel>... cn) {
            return new QueryModel().or(c1, c2, cn);
        }
        public static BooleanProperty<QueryModel> myBoolean() {
            return new QueryModel().myBoolean();
        }
        public static StringProperty<QueryModel> myString(){
            return new QueryModel().myString();
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
    }

    public BooleanProperty<QueryModel> myBoolean() {
        return booleanField(getCurrentMethodName());
    }

    public StringProperty<QueryModel> myString() {
        return stringField(getCurrentMethodName());
    }

    public LongProperty<QueryModel> myLong() {
        return longField(getCurrentMethodName());
    }

    public DoubleProperty<QueryModel> myDouble() {
        return doubleField(getCurrentMethodName());
    }

    public IntegerProperty<QueryModel> myInteger() {
        return integerField(getCurrentMethodName());
    }

    public ShortProperty<QueryModel> myShort() {
        return shortField(getCurrentMethodName());
    }

    public FloatProperty<QueryModel> myFloat() {
        return floatField(getCurrentMethodName());
    }

}
