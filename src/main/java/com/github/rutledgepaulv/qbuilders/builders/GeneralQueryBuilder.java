package com.github.rutledgepaulv.qbuilders.builders;

import com.github.rutledgepaulv.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.qbuilders.nodes.ComparisonNode;

public class GeneralQueryBuilder extends QBuilder<GeneralQueryBuilder> {

    public Condition<GeneralQueryBuilder> passThrough(ComparisonNode node) {
        return condition(node.getField(), node.getOperator(), node.getValues());
    }

}
