package com.github.rutledgepaulv.basic.qbuilders.visitors.advanced;

import com.github.rutledgepaulv.basic.qbuilders.nodes.ComparisonNode;
import com.github.rutledgepaulv.basic.qbuilders.operators.advanced.AdvancedMongoOperator;
import com.github.rutledgepaulv.basic.qbuilders.operators.basic.ComparisonOperator;
import com.github.rutledgepaulv.basic.qbuilders.visitors.basic.BasicMongoVisitor;
import org.springframework.data.mongodb.core.query.Criteria;

public class AdvancedMongoVisitor extends BasicMongoVisitor {

    @Override
    protected Criteria visit(ComparisonNode node) {
        Criteria parent = super.visit(node);
        if(parent != null) {
            return parent;
        }

        ComparisonOperator operator = node.getOperator();

        if(operator.equals(AdvancedMongoOperator.REGEX)) {
            return Criteria.where(node.getField()).regex((String) node.getValues().iterator().next());
        }

        return null;
    }



}
