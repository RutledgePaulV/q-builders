package com.github.rutledgepaulv.basic.qbuilders.visitors.basic;

import com.github.rutledgepaulv.basic.qbuilders.nodes.AndNode;
import com.github.rutledgepaulv.basic.qbuilders.nodes.ComparisonNode;
import com.github.rutledgepaulv.basic.qbuilders.nodes.OrNode;
import com.github.rutledgepaulv.basic.qbuilders.operators.basic.ComparisonOperator;
import com.github.rutledgepaulv.basic.qbuilders.visitors.NodeVisitor;
import org.elasticsearch.index.query.FilterBuilder;
import org.elasticsearch.index.query.FilterBuilders;

import java.util.List;
import java.util.stream.Collectors;

import static org.elasticsearch.index.query.FilterBuilders.*;

public class BasicEsVisitor extends NodeVisitor<FilterBuilder> {

    @Override
    protected FilterBuilder visit(AndNode node) {
        List<FilterBuilder> children = node.getChildren().stream().map(this::visit).collect(Collectors.toList());
        return FilterBuilders.andFilter(children.toArray(new FilterBuilder[children.size()]));
    }

    @Override
    protected FilterBuilder visit(OrNode node) {
        List<FilterBuilder> children = node.getChildren().stream().map(this::visit).collect(Collectors.toList());
        return FilterBuilders.orFilter(children.toArray(new FilterBuilder[children.size()]));
    }

    @Override
    protected FilterBuilder visit(ComparisonNode node) {
        ComparisonOperator operator = node.getOperator();

        if(ComparisonOperator.EQ.equals(operator)) {
            return termFilter(node.getField(), node.getValues().iterator().next());
        } else if(ComparisonOperator.NE.equals(operator)) {
            return notFilter(termFilter(node.getField(), node.getValues().iterator().next()));
        } else if (ComparisonOperator.EX.equals(operator)) {
            if(node.getValues().iterator().next().equals(true)) {
                return existsFilter(node.getField());
            } else {
                return missingFilter(node.getField());
            }
        } else if (ComparisonOperator.GT.equals(operator)) {
            return rangeFilter(node.getField()).gt(node.getValues().iterator().next());
        } else if (ComparisonOperator.LT.equals(operator)) {
            return rangeFilter(node.getField()).lt(node.getValues().iterator().next());
        } else if (ComparisonOperator.GTE.equals(operator)) {
            return rangeFilter(node.getField()).gte(node.getValues().iterator().next());
        } else if (ComparisonOperator.LTE.equals(operator)) {
            return rangeFilter(node.getField()).lte(node.getValues().iterator().next());
        } else if (ComparisonOperator.IN.equals(operator)) {
            return termsFilter(node.getField(), node.getValues());
        } else if (ComparisonOperator.NIN.equals(operator)) {
            return notFilter(termsFilter(node.getField(), node.getValues()));
        }

        return null;
    }

}
