package com.github.rutledgepaulv.basic.qbuilders.visitors.basic;

import com.github.rutledgepaulv.basic.qbuilders.nodes.AndNode;
import com.github.rutledgepaulv.basic.qbuilders.nodes.ComparisonNode;
import com.github.rutledgepaulv.basic.qbuilders.nodes.OrNode;
import com.github.rutledgepaulv.basic.qbuilders.operators.basic.ComparisonOperator;
import com.github.rutledgepaulv.basic.qbuilders.visitors.NodeVisitor;
import org.springframework.data.mongodb.core.query.Criteria;

import java.util.List;
import java.util.stream.Collectors;

import static org.springframework.data.mongodb.core.query.Criteria.where;

public class BasicMongoVisitor extends NodeVisitor<Criteria> {

    @Override
    protected Criteria visit(AndNode node) {
        Criteria criteria = new Criteria();
        List<Criteria> children = node.getChildren().stream()
                .map(this::visit).collect(Collectors.toList());
        return criteria.andOperator(children.toArray(new Criteria[children.size()]));
    }

    @Override
    protected Criteria visit(OrNode node) {
        Criteria criteria = new Criteria();
        List<Criteria> children = node.getChildren().stream()
                .map(this::visit).collect(Collectors.toList());
        return criteria.orOperator(children.toArray(new Criteria[children.size()]));
    }

    @Override
    protected Criteria visit(ComparisonNode node) {

        ComparisonOperator operator = node.getOperator();

        if(ComparisonOperator.EQ.equals(operator)) {
            return where(node.getField()).is(node.getValues().iterator().next());
        } else if(ComparisonOperator.NE.equals(operator)) {
            return where(node.getField()).ne(node.getValues().iterator().next());
        } else if (ComparisonOperator.EX.equals(operator)) {
            return where(node.getField()).exists((Boolean)node.getValues().iterator().next());
        } else if (ComparisonOperator.GT.equals(operator)) {
            return where(node.getField()).gt(node.getValues().iterator().next());
        } else if (ComparisonOperator.LT.equals(operator)) {
            return where(node.getField()).lt(node.getValues().iterator().next());
        } else if (ComparisonOperator.GTE.equals(operator)) {
            return where(node.getField()).gte(node.getValues().iterator().next());
        } else if (ComparisonOperator.LTE.equals(operator)) {
            return where(node.getField()).lte(node.getValues().iterator().next());
        } else if (ComparisonOperator.IN.equals(operator)) {
            return where(node.getField()).in(node.getValues());
        } else if (ComparisonOperator.NIN.equals(operator)) {
            return where(node.getField()).nin(node.getValues());
        }

        return null;

    }

}
