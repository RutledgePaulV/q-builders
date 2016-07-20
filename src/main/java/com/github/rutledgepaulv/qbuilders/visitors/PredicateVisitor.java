/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.visitors.PredicateVisitor
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
import org.apache.commons.lang3.reflect.FieldUtils;

import java.util.Arrays;
import java.util.Collection;
import java.util.Objects;
import java.util.function.BiPredicate;
import java.util.function.Predicate;
import java.util.regex.Pattern;

import static java.util.Arrays.stream;

@SuppressWarnings("WeakerAccess")
public class PredicateVisitor<T> extends AbstractVoidContextNodeVisitor<Predicate<T>> {

    @Override
    protected Predicate<T> visit(AndNode node) {
        return (t) -> node.getChildren().stream().map(this::visitAny).allMatch(p -> p.test(t));
    }

    @Override
    protected Predicate<T> visit(OrNode node) {
        return (t) -> node.getChildren().stream().map(this::visitAny).anyMatch(p -> p.test(t));
    }

    @Override
    protected Predicate<T> visit(ComparisonNode node) {

        ComparisonOperator operator = node.getOperator();

        if(ComparisonOperator.EQ.equals(operator)) {
            return single(node, this::equality);
        } else if(ComparisonOperator.NE.equals(operator)) {
            return single(node, this::inequality);
        } else if (ComparisonOperator.EX.equals(operator)) {
            return ((Boolean)node.getValues().iterator().next()) ? exists(node) : doesNotExist(node);
        } else if (ComparisonOperator.GT.equals(operator)) {
            return single(node, this::greaterThan);
        } else if (ComparisonOperator.LT.equals(operator)) {
            return single(node, this::lessThan);
        } else if (ComparisonOperator.GTE.equals(operator)) {
            return single(node, this::greaterThanOrEqualTo);
        } else if (ComparisonOperator.LTE.equals(operator)) {
            return single(node, this::lessThanOrEqualTo);
        } else if (ComparisonOperator.IN.equals(operator)) {
            return multi(node, this::in);
        } else if (ComparisonOperator.NIN.equals(operator)) {
            return multi(node, this::nin);
        } else if (ComparisonOperator.RE.equals(operator)) {
            return single(node, this::regex);
        } else if (ComparisonOperator.SUB_CONDITION_ANY.equals(operator)) {
            Predicate test = condition(node);
            // subquery condition is ignored because a predicate has already been built.
            return single(node, (fieldValue, subQueryCondition) -> this.subquery(fieldValue, test));
        }

        throw new UnsupportedOperationException("This visitor does not support the operator " + operator + ".");
    }

    protected boolean subquery(Object actual, Predicate<Object> func) {
        if (actual != null && actual.getClass().isArray()) {
            Object[] values = (Object[]) actual;
            return stream(values).anyMatch(func);
        } else if (actual != null && Collection.class.isAssignableFrom(actual.getClass())) {
            Collection<?> values = (Collection<?>) actual;
            return values.stream().anyMatch(func);
        } else {
            throw new IllegalArgumentException("You cannot do a subquery against a single element.");
        }
    }

    protected boolean regex(Object actual, Object query) {
        Predicate<String> test;

        if (query instanceof String) {
            String queryRegex = (String) query;
            test = Pattern.compile(queryRegex).asPredicate();
        } else {
            return false;
        }

        if (actual.getClass().isArray()) {
            String[] values = (String[]) actual;
            return Arrays.stream(values).anyMatch(test);
        } else if (Collection.class.isAssignableFrom(actual.getClass())) {
            Collection<String> values = (Collection<String>) actual;
            return values.stream().anyMatch(test);
        } else if (actual instanceof String) {
            return test.test((String) actual);
        }

        return false;
    }

    protected boolean equality(Object actual, Object query) {
        if(actual != null && actual.getClass().isArray()) {
            Object[] values = (Object[]) actual;
            return stream(values).anyMatch(query::equals);
        } else if (actual != null && Collection.class.isAssignableFrom(actual.getClass())) {
            Collection<?> values = (Collection<?>)actual;
            return values.stream().anyMatch(query::equals);
        } else {
            return query.equals(actual);
        }
    }

    protected boolean inequality(Object actual, Object query) {
        if(actual != null && actual.getClass().isArray()) {
            Object[] values = (Object[]) actual;
            return stream(values).noneMatch(query::equals);
        } else if (actual != null && Collection.class.isAssignableFrom(actual.getClass())) {
            Collection<?> values = (Collection<?>)actual;
            return values.stream().noneMatch(query::equals);
        } else {
            return !query.equals(actual);
        }
    }

    protected boolean nin(Object actual, Collection<?> queries) {
        if(actual != null && actual.getClass().isArray()) {
            Object[] values = (Object[]) actual;
            return stream(values).noneMatch(queries::contains);
        } else if (actual != null &&  Collection.class.isAssignableFrom(actual.getClass())) {
            Collection<?> values = (Collection<?>)actual;
            return values.stream().noneMatch(queries::contains);
        } else {
            return !queries.contains(actual);
        }
    }

    protected boolean in(Object actual, Collection<?> queries) {
        if(actual != null && actual.getClass().isArray()) {
            Object[] values = (Object[]) actual;
            return stream(values).anyMatch(queries::contains);
        } else if (actual != null && Collection.class.isAssignableFrom(actual.getClass())) {
            Collection<?> values = (Collection<?>)actual;
            return values.stream().anyMatch(queries::contains);
        } else {
            return queries.contains(actual);
        }
    }

    protected boolean greaterThan(Object actual, Object query) {
        if(query instanceof Number && actual instanceof Number) {
            return ((Number)actual).doubleValue() > ((Number) query).doubleValue();
        } else if (query instanceof String && actual instanceof String) {
            return ((String)actual).compareTo((String)query) > 0;
        } else {
            throw new UnsupportedOperationException("Incompatible types provided.");
        }
    }

    protected boolean greaterThanOrEqualTo(Object actual, Object query) {
        if(query instanceof Number && actual instanceof Number) {
            return ((Number)actual).doubleValue() >= ((Number) query).doubleValue();
        } else if (query instanceof String && actual instanceof String) {
            return ((String)actual).compareTo((String)query) >= 0;
        } else {
            throw new UnsupportedOperationException("Incompatible types provided.");
        }
    }

    protected boolean lessThan(Object actual, Object query) {
        if(query instanceof Number && actual instanceof Number) {
            return ((Number)actual).doubleValue() < ((Number) query).doubleValue();
        } else if (query instanceof String && actual instanceof String) {
            return ((String)actual).compareTo((String)query) < 0;
        } else {
            throw new UnsupportedOperationException("Incompatible types provided.");
        }
    }

    protected boolean lessThanOrEqualTo(Object actual, Object query) {
        if(query instanceof Number && actual instanceof Number) {
            return ((Number)actual).doubleValue() <= ((Number) query).doubleValue();
        } else if (query instanceof String && actual instanceof String) {
            return ((String)actual).compareTo((String)query) <= 0;
        } else {
            throw new UnsupportedOperationException("Incompatible types provided.");
        }
    }

    private Predicate<T> doesNotExist(ComparisonNode node) {
        return t -> resolveSingleField(t, node.getField().asKey(), node, (one, two) -> Objects.isNull(one));
    }

    private Predicate<T> exists(ComparisonNode node) {
        return t -> resolveSingleField(t, node.getField().asKey(), node, (one, two) -> Objects.nonNull(one));
    }

    private Predicate<T> single(ComparisonNode node, BiPredicate<Object, Object> func) {
        return t -> resolveSingleField(t, node.getField().asKey(), node, func);
    }

    private Predicate<T> multi(ComparisonNode node, BiPredicate<Object, Collection<?>> func) {
        return t -> resolveMultiField(t, node.getField().asKey(), node, func);
    }

    private boolean resolveSingleField(Object root, String field, ComparisonNode node, BiPredicate<Object, Object> func) {
        if(root == null || node.getField() == null) {
            return func.test(null, node.getValues().iterator().next());
        } else {
            String[] splitField = field.split("\\.", 2);
            Object currentField = getFieldValueFromString(root, splitField[0]);
            if(splitField.length == 1) {
                return func.test(currentField, node.getValues().iterator().next());
            } else {
                return recurseSingle(currentField, splitField[1], node, func);
            }
        }
    }

    private boolean recurseSingle(Object root, String field, ComparisonNode node, BiPredicate<Object, Object> func) {

        if(root.getClass().isArray()) {
            return Arrays.stream((Object[])root).anyMatch(t -> resolveSingleField(t, field, node, func));
        }

        if(root instanceof Collection) {
            return ((Collection<Object>)root).stream().anyMatch(t -> resolveSingleField(t, field, node, func));
        }

        return resolveSingleField(root, field, node, func);
    }

    private boolean resolveMultiField(Object root, String field, ComparisonNode node, BiPredicate<Object, Collection<?>> func) {
        if(root == null || node.getField() == null) {
            return func.test(null, node.getValues());
        } else {
            String[] splitField = field.split("\\.", 2);
            Object currentField = getFieldValueFromString(root, splitField[0]);
            if(splitField.length == 1) {
                return func.test(currentField, node.getValues());
            } else {
                return recurseMulti(currentField, splitField[1], node, func);
            }
        }
    }

    private boolean recurseMulti(Object root, String field, ComparisonNode node, BiPredicate<Object, Collection<?>> func) {

        if(root.getClass().isArray()) {
            return Arrays.stream((Object[])root).anyMatch(t -> resolveMultiField(t, field, node, func));
        }

        if(root instanceof Collection) {
            return ((Collection<Object>)root).stream().anyMatch(t -> resolveMultiField(t, field, node, func));
        }

        return resolveMultiField(root, field, node, func);
    }

    private Object getFieldValueFromString(Object o, String s) {
        if(o == null) {
            return null;
        }
        try {
            return FieldUtils.readField(o, s, true);
        } catch (IllegalAccessException e) {
            return null;
        }
    }

}
