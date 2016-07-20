/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.operators.ComparisonOperatorTest
 *  *  *
 *  *  * Copyright (C) 2016 Paul Rutledge <paul.v.rutledge@gmail.com>
 *  *  *
 *  *  * This software may be modified and distributed under the terms
 *  *  * of the MIT license.  See the LICENSE file for details.
 *  *
 *
 */

package com.github.rutledgepaulv.qbuilders.operators;

import com.github.rutledgepaulv.qbuilders.operators.ComparisonOperator;
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
