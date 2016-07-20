/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.operators.ComparisonOperator
 *  *  *
 *  *  * Copyright (C) 2016 Paul Rutledge <paul.v.rutledge@gmail.com>
 *  *  *
 *  *  * This software may be modified and distributed under the terms
 *  *  * of the MIT license.  See the LICENSE file for details.
 *  *
 *
 */

package com.github.rutledgepaulv.qbuilders.operators;

import java.util.Objects;

public final class ComparisonOperator {

    private String representation;

    public ComparisonOperator(String representation) {
        this.representation = representation;
    }

    public static final ComparisonOperator EQ = new ComparisonOperator("EQ");
    public static final ComparisonOperator NE = new ComparisonOperator("NE");
    public static final ComparisonOperator GT = new ComparisonOperator("GT");
    public static final ComparisonOperator LT = new ComparisonOperator("LT");
    public static final ComparisonOperator GTE = new ComparisonOperator("GTE");
    public static final ComparisonOperator LTE = new ComparisonOperator("LTE");
    public static final ComparisonOperator IN = new ComparisonOperator("IN");
    public static final ComparisonOperator NIN = new ComparisonOperator("NIN");
    public static final ComparisonOperator EX = new ComparisonOperator("EX");
    public static final ComparisonOperator RE = new ComparisonOperator("RE");
    public static final ComparisonOperator SUB_CONDITION_ANY = new ComparisonOperator("SUB_CONDITION_ANY");

    @Override
    public final boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        ComparisonOperator that = (ComparisonOperator) o;
        return Objects.equals(representation, that.representation);
    }

    @Override
    public final int hashCode() {
        return Objects.hash(representation);
    }
}
