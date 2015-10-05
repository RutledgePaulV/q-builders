package com.github.rutledgepaulv.visitors;

import com.github.rutledgepaulv.nodes.AndNode;
import com.github.rutledgepaulv.nodes.ComparisonNode;
import com.github.rutledgepaulv.nodes.OrNode;
import org.springframework.data.mongodb.core.query.Criteria;

import java.util.List;
import java.util.stream.Collectors;

import static org.springframework.data.mongodb.core.query.Criteria.where;

public class MongoCriteriaVisitor extends NodeVisitor<Criteria> {

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
        switch(node.getOperator()) {
            case EQ:
                return where(node.getField()).is(node.getValues().iterator().next());
            case NE:
                return where(node.getField()).ne(node.getValues().iterator().next());
            case EX:
                return where(node.getField()).exists((Boolean)node.getValues().iterator().next());
            case GT:
                return where(node.getField()).gt(node.getValues().iterator().next());
            case LT:
                return where(node.getField()).lt(node.getValues().iterator().next());
            case GTE:
                return where(node.getField()).gte(node.getValues().iterator().next());
            case LTE:
                return where(node.getField()).lte(node.getValues().iterator().next());
            case IN:
                return where(node.getField()).in(node.getValues());
            case NIN:
                return where(node.getField()).nin(node.getValues());
        }
        throw new UnsupportedOperationException("You used an unknown operator.");
    }

}
