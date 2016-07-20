/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.visitors.ElasticsearchVisitor
 *  *  *
 *  *  * Copyright (C) 2016 Paul Rutledge <paul.v.rutledge@gmail.com>
 *  *  *
 *  *  * This software may be modified and distributed under the terms
 *  *  * of the MIT license.  See the LICENSE file for details.
 *  *
 *
 */

package com.github.rutledgepaulv.qbuilders.visitors;

import com.github.rutledgepaulv.qbuilders.nodes.*;
import com.github.rutledgepaulv.qbuilders.operators.ComparisonOperator;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilder;

import java.util.Collection;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.elasticsearch.index.query.QueryBuilders.*;


@SuppressWarnings("WeakerAccess")
public class ElasticsearchVisitor extends ContextualNodeVisitor<QueryBuilder, ElasticsearchVisitor.Context> {

    public static class Context {
        private boolean originatedAsNestedQuery = false;
        public Context(){}
        public Context(boolean originatedAsNestedQuery) {
            this.originatedAsNestedQuery = originatedAsNestedQuery;
        }
    }

    protected final Function<Object, Object> normalizer;

    public ElasticsearchVisitor() {
        this(Function.identity());
    }

    public ElasticsearchVisitor(Function<Object, Object> normalizer) {
        this.normalizer = normalizer;
    }

    @Override
    protected QueryBuilder visit(AndNode node, Context context) {
        BoolQueryBuilder parent = boolQuery();
        node.getChildren().stream().map(child -> visitAny(child, context)).forEach(parent::must);
        return parent;
    }

    @Override
    protected QueryBuilder visit(OrNode node, Context context) {
        BoolQueryBuilder parent = boolQuery();
        node.getChildren().stream().map(child -> visitAny(child, context)).forEach(parent::should);
        return parent;
    }

    @Override
    protected QueryBuilder visit(ComparisonNode node, Context context) {
        ComparisonOperator operator = node.getOperator();
        
        Collection<?> values = node.getValues().stream().map(normalizer).collect(Collectors.toList());

        String field = context.originatedAsNestedQuery ? node.getField().asFullyQualifiedKey() : node.getField().asKey();

        if (ComparisonOperator.EQ.equals(operator)) {
            return termQuery(field, single(values));
        } else if (ComparisonOperator.NE.equals(operator)) {
            return boolQuery().mustNot(termQuery(field, single(values)));
        } else if (ComparisonOperator.EX.equals(operator)) {
            if (single(values).equals(true)) {
                return existsQuery(field);
            } else {
                return boolQuery().mustNot(existsQuery(field));
            }
        } else if (ComparisonOperator.GT.equals(operator)) {
            return rangeQuery(field).gt(single(values));
        } else if (ComparisonOperator.LT.equals(operator)) {
            return rangeQuery(field).lt(single(values));
        } else if (ComparisonOperator.GTE.equals(operator)) {
            return rangeQuery(field).gte(single(values));
        } else if (ComparisonOperator.LTE.equals(operator)) {
            return rangeQuery(field).lte(single(values));
        } else if (ComparisonOperator.IN.equals(operator)) {
            return termsQuery(field, values);
        } else if (ComparisonOperator.NIN.equals(operator)) {
            return boolQuery().mustNot(termsQuery(field, values));
        } else if (ComparisonOperator.RE.equals(operator)) {
            return regexpQuery(field, (String) single(values));
        } else if (ComparisonOperator.SUB_CONDITION_ANY.equals(operator)) {
            return nestedQuery(field, condition(node, new Context(true)));
        }

        throw new UnsupportedOperationException("This visitor does not support the operator " + operator + ".");
    }


}
