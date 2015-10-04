package com.github.rutledgepaulv;

import com.github.rutledgepaulv.builders.QueryBuilder;
import com.github.rutledgepaulv.conditions.CompleteCondition;
import com.github.rutledgepaulv.properties.concrete.LongProperty;
import com.github.rutledgepaulv.properties.concrete.StringProperty;
import com.github.rutledgepaulv.visitors.RSQLVisitor;
import org.junit.Test;

public class QueryBuilderTest {


    private class Builder extends QueryBuilder<Builder> {

        public StringProperty<Builder> myString() {
            return stringField("myString");
        }

        public LongProperty<Builder> myLong() {
            return longField("myLong");
        }

    }


    @Test
    public void test() {
        CompleteCondition<Builder> condition = new Builder().myString().eq("Thing").and().myLong().doesNotExist();
        String query = condition.query(new RSQLVisitor());
        System.out.println(query);
    }

}
