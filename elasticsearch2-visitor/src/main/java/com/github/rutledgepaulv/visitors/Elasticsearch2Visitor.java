package com.github.rutledgepaulv.visitors;


import com.github.rutledgepaulv.qbuilders.nodes.AndNode;
import com.github.rutledgepaulv.qbuilders.nodes.ComparisonNode;
import com.github.rutledgepaulv.qbuilders.nodes.OrNode;
import com.github.rutledgepaulv.qbuilders.operators.ComparisonOperator;
import com.github.rutledgepaulv.qbuilders.visitors.NodeVisitor;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;

import java.util.Collection;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.elasticsearch.index.query.QueryBuilders.*;


@SuppressWarnings("WeakerAccess")
public class Elasticsearch2Visitor extends NodeVisitor<QueryBuilder> {

    protected static final Function<Object, Object> IDENTITY = object -> object;
    protected final Function<Object, Object> normalizer;

    public Elasticsearch2Visitor() {
        this(IDENTITY);
    }

    public Elasticsearch2Visitor(Function<Object, Object> normalizer) {
        this.normalizer = normalizer;
    }

    @Override
    protected QueryBuilder visit(AndNode node) {
        BoolQueryBuilder parent = boolQuery();
        node.getChildren().stream().map(this::visitAny).forEach(parent::must);
        return parent;
    }

    @Override
    protected QueryBuilder visit(OrNode node) {
        BoolQueryBuilder parent = boolQuery();
        node.getChildren().stream().map(this::visitAny).forEach(parent::should);
        return parent;
    }

    @Override
    protected QueryBuilder visit(ComparisonNode node) {
        ComparisonOperator operator = node.getOperator();

        Collection<?> values = node.getValues().stream().map(normalizer).collect(Collectors.toList());

        if (ComparisonOperator.EQ.equals(operator)) {
            return QueryBuilders.termQuery(node.getField(), single(values));
        } else if (ComparisonOperator.NE.equals(operator)) {
            return boolQuery().mustNot(QueryBuilders.termQuery(node.getField(), single(values)));
        } else if (ComparisonOperator.EX.equals(operator)) {
            if (single(values).equals(true)) {
                return existsQuery(node.getField());
            } else {
                return boolQuery().mustNot(existsQuery(node.getField()));
            }
        } else if (ComparisonOperator.GT.equals(operator)) {
            return rangeQuery(node.getField()).gt(single(values));
        } else if (ComparisonOperator.LT.equals(operator)) {
            return rangeQuery(node.getField()).lt(single(values));
        } else if (ComparisonOperator.GTE.equals(operator)) {
            return rangeQuery(node.getField()).gte(single(values));
        } else if (ComparisonOperator.LTE.equals(operator)) {
            return rangeQuery(node.getField()).lte(single(values));
        } else if (ComparisonOperator.IN.equals(operator)) {
            return QueryBuilders.termsQuery(node.getField(), values);
        } else if (ComparisonOperator.NIN.equals(operator)) {
            return boolQuery().mustNot(QueryBuilders.termsQuery(node.getField(), values));
        } else if (ComparisonOperator.SUB_CONDITION_ANY.equals(operator)) {
            return nestedQuery(node.getField(), condition(node));
        }

        throw new UnsupportedOperationException("This visitor does not support the operator " + operator + ".");
    }

}
