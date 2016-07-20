/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.builders.GeneralQueryBuilder
 *  *  *
 *  *  * Copyright (C) 2016 Paul Rutledge <paul.v.rutledge@gmail.com>
 *  *  *
 *  *  * This software may be modified and distributed under the terms
 *  *  * of the MIT license.  See the LICENSE file for details.
 *  *
 *
 */

package com.github.rutledgepaulv.qbuilders.builders;

import com.github.rutledgepaulv.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.qbuilders.nodes.ComparisonNode;

public class GeneralQueryBuilder extends QBuilder<GeneralQueryBuilder> {

    public Condition<GeneralQueryBuilder> passThrough(ComparisonNode node) {
        return condition(node.getField(), node.getOperator(), node.getValues());
    }

}
