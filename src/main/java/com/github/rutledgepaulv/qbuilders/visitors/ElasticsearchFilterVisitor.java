package com.github.rutledgepaulv.qbuilders.visitors;

import com.github.rutledgepaulv.qbuilders.nodes.AndNode;
import com.github.rutledgepaulv.qbuilders.nodes.ComparisonNode;
import com.github.rutledgepaulv.qbuilders.nodes.OrNode;
import org.elasticsearch.index.query.FilterBuilder;
import org.elasticsearch.index.query.FilterBuilders;

import java.util.List;
import java.util.stream.Collectors;

import static org.elasticsearch.index.query.FilterBuilders.*;

public class ElasticsearchFilterVisitor extends NodeVisitor<FilterBuilder> {

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
        switch(node.getOperator()) {
            case EQ:
                return termFilter(node.getField(), node.getValues().iterator().next());
            case NE:
                return notFilter(termFilter(node.getField(), node.getValues().iterator().next()));
            case EX:
                if(node.getValues().iterator().next().equals(true)) {
                    return existsFilter(node.getField());
                } else {
                    return missingFilter(node.getField());
                }
            case GT:
                return rangeFilter(node.getField()).gt(node.getValues().iterator().next());
            case LT:
                return rangeFilter(node.getField()).lt(node.getValues().iterator().next());
            case GTE:
                return rangeFilter(node.getField()).gte(node.getValues().iterator().next());
            case LTE:
                return rangeFilter(node.getField()).lte(node.getValues().iterator().next());
            case IN:
                return termsFilter(node.getField(), node.getValues());
            case NIN:
                return notFilter(termsFilter(node.getField(), node.getValues()));
        }
        throw new UnsupportedOperationException("You used an invalid operator.");
    }

}
