package com.github.rutledgepaulv.qbuilders.visitors;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.rutledgepaulv.qbuilders.nodes.AndNode;
import com.github.rutledgepaulv.qbuilders.nodes.ComparisonNode;
import com.github.rutledgepaulv.qbuilders.nodes.OrNode;
import com.github.rutledgepaulv.qbuilders.utilities.PathUtils;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.function.BiFunction;
import java.util.function.Predicate;

// TODO: replace json nodes with more efficient object graph library (I'm working on one, but it isn't ready)
public class PredicateVisitor<T> extends NodeVisitor<Predicate<T>> {

    private static final ObjectMapper mapper = new ObjectMapper();

    @Override
    protected Predicate<T> visit(AndNode node) {
        return (t) -> node.getChildren().stream().map(this::visit).allMatch(p -> p.test(t));
    }

    @Override
    protected Predicate<T> visit(OrNode node) {
        return (t) -> node.getChildren().stream().map(this::visit).anyMatch(p -> p.test(t));
    }

    @Override
    protected Predicate<T> visit(ComparisonNode node) {

        switch (node.getOperator()) {
            case EQ:
                return single(node, this::equality);
            case NE:
                return single(node, this::inequality);
            case EX:
                return ((Boolean)node.getValues().iterator().next()) ? exists(node) : doesNotExist(node);
            case GT:
                return single(node, this::greaterThan);
            case GTE:
                return single(node, this::greaterThanOrEqualTo);
            case LT:
                return single(node, this::lessThan);
            case LTE:
                return single(node, this::lessThanOrEqualTo);
            case IN:
                return multi(node, this::in);
            case NIN:
                return multi(node, this::nin);
        }

        throw new UnsupportedOperationException("Unsupported operator.");
    }

    private boolean equality(Object actual, Object query) {
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

    private boolean inequality(Object actual, Object query) {
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

    private boolean nin(Object actual, Collection<?> queries) {
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

    private boolean in(Object actual, Collection<?> queries) {
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

    private boolean greaterThan(Object actual, Object query) {
        if(query instanceof Number && actual instanceof Number) {
            return ((Number)actual).doubleValue() > ((Number) query).doubleValue();
        } else if (query instanceof String && actual instanceof String) {
            return ((String)actual).compareTo((String)query) > 0;
        } else {
            throw new UnsupportedOperationException("Incompatible types provided.");
        }
    }

    private boolean greaterThanOrEqualTo(Object actual, Object query) {
        if(query instanceof Number && actual instanceof Number) {
            return ((Number)actual).doubleValue() >= ((Number) query).doubleValue();
        } else if (query instanceof String && actual instanceof String) {
            return ((String)actual).compareTo((String)query) >= 0;
        } else {
            throw new UnsupportedOperationException("Incompatible types provided.");
        }
    }

    private boolean lessThan(Object actual, Object query) {
        if(query instanceof Number && actual instanceof Number) {
            return ((Number)actual).doubleValue() < ((Number) query).doubleValue();
        } else if (query instanceof String && actual instanceof String) {
            return ((String)actual).compareTo((String)query) < 0;
        } else {
            throw new UnsupportedOperationException("Incompatible types provided.");
        }
    }

    private boolean lessThanOrEqualTo(Object actual, Object query) {
        if(query instanceof Number && actual instanceof Number) {
            return ((Number)actual).doubleValue() <= ((Number) query).doubleValue();
        } else if (query instanceof String && actual instanceof String) {
            return ((String)actual).compareTo((String)query) <= 0;
        } else {
            throw new UnsupportedOperationException("Incompatible types provided.");
        }
    }


    private Predicate<T> doesNotExist(ComparisonNode node) {
        return t -> !exists(node.getField(), t);
    }

    private Predicate<T> exists(ComparisonNode node) {
        return t -> exists(node.getField(), t);
    }

    private Predicate<T> single(ComparisonNode node, BiFunction<Object, Object, Boolean> func) {
        return t -> func.apply(obj(node.getField(), t), node.getValues().iterator().next());
    }

    private Predicate<T> multi(ComparisonNode node, BiFunction<Object, Collection<?>, Boolean> func) {
        return t -> func.apply(obj(node.getField(), t), node.getValues());
    }

    private Object obj(String fieldName, Object actual) {
        JsonNode node = mapper.valueToTree(actual);
        Iterator<String> iter = PathUtils.iterator(fieldName);

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


    private boolean exists(String fieldName, Object actual) {
        JsonNode node = mapper.valueToTree(actual);
        Iterator<String> iter = PathUtils.iterator(fieldName);

        while(iter.hasNext()) {
            node = node.path(iter.next());
        }

        return !node.isMissingNode() && !node.isNull();
    }

}
