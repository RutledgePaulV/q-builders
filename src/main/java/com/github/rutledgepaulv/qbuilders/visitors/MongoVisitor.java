/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.visitors.MongoVisitor
 *  *  *
 *  *  * Copyright (C) 2016 Paul Rutledge <paul.v.rutledge@gmail.com>
 *  *  *
 *  *  * This software may be modified and distributed under the terms
 *  *  * of the MIT license.  See the LICENSE file for details.
 *  *
 *
 */

package com.github.rutledgepaulv.qbuilders.visitors;

import com.github.rutledgepaulv.qbuilders.nodes.AndNode;
import com.github.rutledgepaulv.qbuilders.nodes.ComparisonNode;
import com.github.rutledgepaulv.qbuilders.nodes.OrNode;
import com.github.rutledgepaulv.qbuilders.operators.ComparisonOperator;
import org.springframework.data.mongodb.core.query.Criteria;

import java.sql.Date;
import java.time.Instant;
import java.util.Collection;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.springframework.data.mongodb.core.query.Criteria.where;

@SuppressWarnings("WeakerAccess")
public class MongoVisitor extends AbstractVoidContextNodeVisitor<Criteria> {

    protected final Function<Object, Object> normalizer;

    public MongoVisitor() {
        this(new DefaultNormalizer());
    }

    public MongoVisitor(Function<Object, Object> normalizer) {
        this.normalizer = normalizer;
    }

    @Override
    protected Criteria visit(AndNode node) {
        Criteria criteria = new Criteria();
        List<Criteria> children = node.getChildren().stream()
                .map(this::visitAny).collect(Collectors.toList());
        return criteria.andOperator(children.toArray(new Criteria[children.size()]));
    }

    @Override
    protected Criteria visit(OrNode node) {
        Criteria criteria = new Criteria();
        List<Criteria> children = node.getChildren().stream()
                .map(this::visitAny).collect(Collectors.toList());
        return criteria.orOperator(children.toArray(new Criteria[children.size()]));
    }

    @Override
    protected Criteria visit(ComparisonNode node) {

        ComparisonOperator operator = node.getOperator();

        Collection<?> values = node.getValues().stream().map(normalizer).collect(Collectors.toList());
        String field = node.getField().asKey();

        if(ComparisonOperator.EQ.equals(operator)) {
            return where(field).is(single(values));
        } else if(ComparisonOperator.NE.equals(operator)) {
            return where(field).ne(single(values));
        } else if (ComparisonOperator.EX.equals(operator)) {
            return where(field).exists((Boolean)single(values));
        } else if (ComparisonOperator.GT.equals(operator)) {
            return where(field).gt(single(values));
        } else if (ComparisonOperator.LT.equals(operator)) {
            return where(field).lt(single(values));
        } else if (ComparisonOperator.GTE.equals(operator)) {
            return where(field).gte(single(values));
        } else if (ComparisonOperator.LTE.equals(operator)) {
            return where(field).lte(single(values));
        } else if (ComparisonOperator.IN.equals(operator)) {
            return where(field).in(values);
        } else if (ComparisonOperator.NIN.equals(operator)) {
            return where(field).nin(values);
        } else if (ComparisonOperator.RE.equals(operator)) {
            return where(field).regex((String)single(values));
        } else if (ComparisonOperator.SUB_CONDITION_ANY.equals(operator)) {
            return where(field).elemMatch(condition(node));
        }

        throw new UnsupportedOperationException("This visitor does not support the operator " + operator + ".");
    }


    protected static class DefaultNormalizer implements Function<Object, Object> {

        @Override
        public Object apply(Object o) {
            if(o instanceof Instant) {
                return Date.from((Instant) o);
            }
            return o;
        }

    }


}
