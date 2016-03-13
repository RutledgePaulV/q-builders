package com.github.rutledgepaulv.visitors;

import com.github.rutledgepaulv.qbuilders.nodes.AndNode;
import com.github.rutledgepaulv.qbuilders.nodes.ComparisonNode;
import com.github.rutledgepaulv.qbuilders.nodes.OrNode;
import com.github.rutledgepaulv.qbuilders.operators.ComparisonOperator;
import com.github.rutledgepaulv.qbuilders.visitors.NodeVisitor;
import org.elasticsearch.index.query.FilterBuilder;
import org.elasticsearch.index.query.FilterBuilders;

import java.time.Instant;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import static org.elasticsearch.index.query.FilterBuilders.*;

public class Elasticsearch1Visitor extends NodeVisitor<FilterBuilder> {

    @Override
    protected FilterBuilder visit(AndNode node) {
        List<FilterBuilder> children = node.getChildren().stream().map(this::visitAny).collect(Collectors.toList());
        return FilterBuilders.andFilter(children.toArray(new FilterBuilder[children.size()]));
    }

    @Override
    protected FilterBuilder visit(OrNode node) {
        List<FilterBuilder> children = node.getChildren().stream().map(this::visitAny).collect(Collectors.toList());
        return FilterBuilders.orFilter(children.toArray(new FilterBuilder[children.size()]));
    }

    @Override
    protected FilterBuilder visit(ComparisonNode node) {
        ComparisonOperator operator = node.getOperator();

        Collection<?> values = node.getValues().stream()
                .map(this::normalize).collect(Collectors.toList());

        if(ComparisonOperator.EQ.equals(operator)) {
            return termFilter(node.getField(), single(values));
        } else if(ComparisonOperator.NE.equals(operator)) {
            return notFilter(termFilter(node.getField(), single(values)));
        } else if (ComparisonOperator.EX.equals(operator)) {
            if(single(values).equals(true)) {
                return existsFilter(node.getField());
            } else {
                return missingFilter(node.getField());
            }
        } else if (ComparisonOperator.GT.equals(operator)) {
            return rangeFilter(node.getField()).gt(single(values));
        } else if (ComparisonOperator.LT.equals(operator)) {
            return rangeFilter(node.getField()).lt(single(values));
        } else if (ComparisonOperator.GTE.equals(operator)) {
            return rangeFilter(node.getField()).gte(single(values));
        } else if (ComparisonOperator.LTE.equals(operator)) {
            return rangeFilter(node.getField()).lte(single(values));
        } else if (ComparisonOperator.IN.equals(operator)) {
            return termsFilter(node.getField(), values);
        } else if (ComparisonOperator.NIN.equals(operator)) {
            return notFilter(termsFilter(node.getField(), values));
        } else if (ComparisonOperator.SUB_CONDITION_ANY.equals(operator)) {
            return nestedFilter(node.getField(), condition(node));
        }

        throw new UnsupportedOperationException("This visitor does not support the operator " + operator + ".");
    }


    protected Object single(Collection<?> values) {
        if(!values.isEmpty()) {
            return values.iterator().next();
        } else {
            throw new IllegalArgumentException("You must provide a query value for the condition.");
        }
    }

    protected Object normalize(Object value) {

        if(value instanceof Instant) {
            return Date.from((Instant) value);
        }

        return value;
    }

}
