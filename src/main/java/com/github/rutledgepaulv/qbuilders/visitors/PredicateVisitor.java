package com.github.rutledgepaulv.qbuilders.visitors;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.rutledgepaulv.qbuilders.nodes.AndNode;
import com.github.rutledgepaulv.qbuilders.nodes.ComparisonNode;
import com.github.rutledgepaulv.qbuilders.nodes.OrNode;
import com.github.rutledgepaulv.qbuilders.operators.ComparisonOperator;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.function.BiFunction;
import java.util.function.Predicate;

@SuppressWarnings("WeakerAccess")
public class PredicateVisitor<T> extends AbstractVoidContextNodeVisitor<Predicate<T>> {

    protected static final ObjectMapper mapper = new ObjectMapper();

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
            return Arrays.stream(values).anyMatch(func);
        } else if (actual != null && Collection.class.isAssignableFrom(actual.getClass())) {
            Collection<?> values = (Collection<?>) actual;
            return values.stream().anyMatch(func);
        } else {
            throw new IllegalArgumentException("You cannot do a subquery against a single element.");
        }
    }


    protected boolean equality(Object actual, Object query) {
        if(actual != null && actual.getClass().isArray()) {
            Object[] values = (Object[]) actual;
            return Arrays.stream(values).anyMatch(query::equals);
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
            return Arrays.stream(values).noneMatch(query::equals);
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
            return Arrays.stream(values).noneMatch(queries::contains);
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
            return Arrays.stream(values).anyMatch(queries::contains);
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


    protected Predicate<T> doesNotExist(ComparisonNode node) {
        return t -> !exists(node.getField().asKey(), t);
    }

    protected Predicate<T> exists(ComparisonNode node) {
        return t -> exists(node.getField().asKey(), t);
    }

    protected Predicate<T> single(ComparisonNode node, BiFunction<Object, Object, Boolean> func) {
        return t -> func.apply(obj(node.getField().asKey(), t), node.getValues().iterator().next());
    }

    protected Predicate<T> multi(ComparisonNode node, BiFunction<Object, Collection<?>, Boolean> func) {
        return t -> func.apply(obj(node.getField().asKey(), t), node.getValues());
    }

    protected Object obj(String fieldName, Object actual) {
        JsonNode node = mapper.valueToTree(actual);

        Iterator<String> iter = Arrays.stream(fieldName.split("\\.")).iterator();

        while(iter.hasNext()) {
            node = node.path(iter.next());
        }

        if(node.isValueNode() || node.isArray()) {
            try {
                return mapper.treeToValue(node, Object.class);
            } catch (JsonProcessingException ignored) {}
        }

        throw new UnsupportedOperationException("Referenced an unresolvable field.");
    }


    protected boolean exists(String fieldName, Object actual) {
        JsonNode node = mapper.valueToTree(actual);

        Iterator<String> iter = Arrays.stream(fieldName.split("\\.")).iterator();

        while(iter.hasNext()) {
            node = node.path(iter.next());
        }

        return !node.isMissingNode() && !node.isNull();
    }

}
