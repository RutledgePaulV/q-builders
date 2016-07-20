/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.delegates.concrete.ConditionPropertyDelegate
 *  *  *
 *  *  * Copyright (C) 2016 Paul Rutledge <paul.v.rutledge@gmail.com>
 *  *  *
 *  *  * This software may be modified and distributed under the terms
 *  *  * of the MIT license.  See the LICENSE file for details.
 *  *
 *
 */

package com.github.rutledgepaulv.qbuilders.delegates.concrete;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.qbuilders.delegates.virtual.PropertyDelegate;
import com.github.rutledgepaulv.qbuilders.nodes.*;
import com.github.rutledgepaulv.qbuilders.operators.ComparisonOperator;
import com.github.rutledgepaulv.qbuilders.properties.concrete.ConditionProperty;
import com.github.rutledgepaulv.qbuilders.structures.FieldPath;
import com.github.rutledgepaulv.qbuilders.visitors.AbstractVoidContextNodeVisitor;

import java.util.Collections;

public final class ConditionPropertyDelegate<T extends QBuilder<T>, S extends QBuilder<S>>
        extends PropertyDelegate<T> implements ConditionProperty<T, S> {

    public ConditionPropertyDelegate(FieldPath field, T canonical) {
        super(field, canonical);
    }

    @Override
    public Condition<T> any(Condition<S> condition) {

        final LogicalNode root = ((ConditionDelegate) condition).getRootNode();

        // prepend this field to all of the fields in the subtree
        root.visit(new NamespacingVisitor(getField()));

        return condition(getField(), ComparisonOperator.SUB_CONDITION_ANY, Collections.singleton(condition));
    }

    private class NamespacingVisitor extends AbstractVoidContextNodeVisitor<Void> {

        private FieldPath parent;

        public NamespacingVisitor(FieldPath parent) {
            this.parent = parent;
        }

        @Override
        protected Void visit(AndNode node) {
            node.getChildren().stream().forEach(this::visitAny);
            return null;
        }

        @Override
        protected Void visit(OrNode node) {
            node.getChildren().stream().forEach(this::visitAny);
            return null;
        }

        @Override
        protected Void visit(ComparisonNode node) {
            node.setField(node.getField().prepend(parent));
            return null;
        }
    }

}
