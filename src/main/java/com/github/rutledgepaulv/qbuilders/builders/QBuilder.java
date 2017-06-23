/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.builders.QBuilder
 *  *  *
 *  *  * Copyright (C) 2016 Paul Rutledge <paul.v.rutledge@gmail.com>
 *  *  *
 *  *  * This software may be modified and distributed under the terms
 *  *  * of the MIT license.  See the LICENSE file for details.
 *  *
 *
 */

package com.github.rutledgepaulv.qbuilders.builders;

import com.github.rutledgepaulv.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.qbuilders.conditions.Partial;
import com.github.rutledgepaulv.qbuilders.delegates.concrete.*;
import com.github.rutledgepaulv.qbuilders.delegates.virtual.Delegate;
import com.github.rutledgepaulv.qbuilders.delegates.virtual.PropertyDelegate;
import com.github.rutledgepaulv.qbuilders.nodes.*;
import com.github.rutledgepaulv.qbuilders.operators.ComparisonOperator;
import com.github.rutledgepaulv.qbuilders.properties.concrete.*;
import com.github.rutledgepaulv.qbuilders.properties.virtual.Property;
import com.github.rutledgepaulv.qbuilders.structures.FieldPath;
import com.github.rutledgepaulv.qbuilders.utilities.ObjectUtils;
import com.github.rutledgepaulv.qbuilders.visitors.ContextualNodeVisitor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import static java.util.Arrays.asList;

/**
 * The single class that can be used to construct an abstract representation of a query. Designed
 * to be extended for each domain model that might be queried against, with each field exposed as
 * a property condition builder.
 *
 * @param <T> The final type of the builder, used for a fluid chaining interface.
 */
@SuppressWarnings("unchecked")
public class QBuilder<T extends QBuilder<T>> implements Partial<T> {

    protected LogicalNode root;
    protected LogicalNode current;

    public QBuilder() {
        root = current = new OrNode();
    }

    public final <S extends Enum<S>> EnumProperty<T,S> enumeration(String field) {
        return prop(field, EnumPropertyDelegate.class, EnumProperty.class);
    }

    public final BooleanProperty<T> bool(String field) {
        return prop(field, BooleanPropertyDelegate.class, BooleanProperty.class);
    }

    public final StringProperty<T> string(String field) {
        return prop(field, StringPropertyDelegate.class, StringProperty.class);
    }

    public final ShortProperty<T> shortNum(String field) {
        return prop(field, ShortPropertyDelegate.class, ShortProperty.class);
    }

    public final IntegerProperty<T> intNum(String field) {
        return prop(field, IntegerPropertyDelegate.class, IntegerProperty.class);
    }

    public final LongProperty<T> longNum(String field) {
        return prop(field, LongPropertyDelegate.class, LongProperty.class);
    }

    public final FloatProperty<T> floatNum(String field) {
        return prop(field, FloatPropertyDelegate.class, FloatProperty.class);
    }

    public final DoubleProperty<T> doubleNum(String field) {
        return prop(field, DoublePropertyDelegate.class, DoubleProperty.class);
    }

    public final InstantProperty<T> instant(String field) {
        return prop(field, InstantPropertyDelegate.class, InstantProperty.class);
    }

    public final <S extends QBuilder<S>> ConditionProperty<T, S> condition(String field) {
        return prop(field, ConditionPropertyDelegate.class, ConditionProperty.class);
    }

    protected final <S extends PropertyDelegate<T>, Q extends Property<T>> Q prop(String field, Class<S> delegate, Class<Q> inter) {
        if(!inter.isAssignableFrom(delegate)) {
            throw new IllegalArgumentException("Must provide a delegate that implements the interface to be returned.");
        }

        return (Q) ObjectUtils.init(delegate, new FieldPath(field), self());
    }

    @SafeVarargs
    public final Condition<T> and(Condition<T> c1, Condition<T> c2, Condition<T>... cn) {
        List<Condition<T>> conditions = new ArrayList<>();
        conditions.addAll(asList(c1,c2));
        conditions.addAll(asList(cn));
        return and(conditions);
    }

    @SafeVarargs
    public final Condition<T> or(Condition<T> c1, Condition<T> c2, Condition<T>... cn) {
        List<Condition<T>> conditions = new ArrayList<>();
        conditions.addAll(asList(c1,c2));
        conditions.addAll(asList(cn));
        return or(conditions);
    }

    public final Condition<T> and(List<Condition<T>> conditions) {
        return combine(conditions, AndNode.class);
    }

    public final Condition<T> or(List<Condition<T>> conditions) {
        return combine(conditions, OrNode.class);
    }


    private <S extends LogicalNode> Condition<T> combine(List<Condition<T>> conditions, Class<S> type) {

        List<AbstractNode> children = conditions.stream()
                .map(condition -> ((QBuilder<T>) condition).self().current)
                .collect(Collectors.toList());

        S node = ObjectUtils.init(type, self().current, children);
        self().current.getChildren().add(node);

        return new ConditionDelegate(self());
    }

    /**
     * Call this method to add a condition to the current logical node of the underlying query tree.
     *
     * @param field The field that this condition belongs to.
     * @param operator The operator indicating how the values provided should be interpreted against the field.
     * @param values The values to use in the comparison against the value of the field.
     *
     * @return A completed {@link Condition} that can be built into a query or logically composed into other conditions.
     */
    protected final Condition<T> condition(FieldPath field, ComparisonOperator operator, Collection<?> values) {
        ComparisonNode node = new ComparisonNode(self().current);

        node.setField(field);
        node.setOperator(operator);
        node.setValues(values);

        self().current.getChildren().add(node);
        return new ConditionDelegate(self());
    }


    /**
     * Since we have delegate classes that extend this class but not its potential end-user imposed subclasses
     * we instead pass the original instance of whatever the final QBuilder class is around as
     * a delegate which each view calls for any operations instead of calling 'this' thereby providing type safe
     * compatibility with extensions.
     *
     * @return The instance that should be modified on actions.
     */
    protected T self() {
        return (T) this;
    }


    /**
     * A delegate view of this builder that represents a logically complete condition. A logically complete
     * condition can either be directly built into a query or it can be composed with other conditions in
     * the form of 'AND' or 'OR'
     */
    protected final class ConditionDelegate extends Delegate<T> implements Condition<T> {

        private ConditionDelegate(T canonical) {
            super(canonical);
        }

        public final T and() {
            QBuilder<T> self = self();
            LogicalNode current = self.current;

            if(!(current instanceof AndNode)) {
                List<AbstractNode> children = new ArrayList<>();
                children.add(current);
                AndNode node = new AndNode(current.getParent(), children);

                // referential comparison intended.
                if (current == self.root) {
                    self.root = node;
                }

                self.current = node;
            }


            return (T) self;
        }

        public final T or() {
            QBuilder<T> self = self();
            LogicalNode current = self.current;

            if(!(current instanceof OrNode)) {
                List<AbstractNode> children = new ArrayList<>();
                children.add(current);
                OrNode node = new OrNode(current.getParent(), children);

                // referential comparison intended.
                if (current == self.root) {
                    self.root = node;
                }
                self.current = node;
            }

            return (T) self;
        }

        public final <Q> Q query(ContextualNodeVisitor<Q,Void> visitor) {
            QBuilder<T> self = self();
            return self.root.visit(visitor);
        }

        public final <Q,S> Q query(ContextualNodeVisitor<Q,S> visitor, S context) {
            QBuilder<T> self = self();
            return self.root.visit(visitor, context);
        }

        public final LogicalNode getRootNode() {
            return self().root;
        }
    }


}
