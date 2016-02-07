package com.github.rutledgepaulv.qbuilders.operators.basic;

import org.junit.Test;

import static org.junit.Assert.assertNotEquals;

public class ComparisonOperatorTest {


    @Test
    public void testEquals() throws Exception {
        ComparisonOperator operator1 = new ComparisonOperator(null);
        ComparisonOperator operator2 = new ComparisonOperator("dogs");
        assertNotEquals(operator1, operator2);
    }


    @Test
    public void testHashCodes() {
        ComparisonOperator operator1 = new ComparisonOperator(null);
        ComparisonOperator operator2 = new ComparisonOperator("cats");
        assertNotEquals(operator1.hashCode(), operator2.hashCode());
    }

}
