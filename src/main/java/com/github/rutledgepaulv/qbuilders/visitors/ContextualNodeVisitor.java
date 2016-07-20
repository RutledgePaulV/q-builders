/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.visitors.ContextualNodeVisitor
 *  *  *
 *  *  * Copyright (C) 2016 Paul Rutledge <paul.v.rutledge@gmail.com>
 *  *  *
 *  *  * This software may be modified and distributed under the terms
 *  *  * of the MIT license.  See the LICENSE file for details.
 *  *
 *
 */

package com.github.rutledgepaulv.qbuilders.visitors;


import com.github.rutledgepaulv.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.qbuilders.nodes.*;
import com.github.rutledgepaulv.qbuilders.operators.ComparisonOperator;

import java.util.Collection;

@SuppressWarnings("ConstantConditions")
public abstract class ContextualNodeVisitor<T,S> {

    protected abstract T visit(AndNode node, S context);

    protected abstract T visit(OrNode node, S context);

    protected abstract T visit(ComparisonNode node, S context);

    /**
     * Build a comparison node value into a visited value so that
     * it can be composed into the larger query being built.
     *
     * @param node The node with a condition argument to build into a visited value.
     *
     * @return The visited value.
     */
    protected T condition(ComparisonNode node,S context) {
        if(!node.getOperator().equals(ComparisonOperator.SUB_CONDITION_ANY)) {
            throw new IllegalArgumentException("You can only build a condition for sub-condition operator nodes.");
        }

        Object sub = node.getValues().iterator().next();

        // support either submitting a tree node in which case handle visiting it for
        // them, or submit a Condition representing a wrapper around that tree in which
        // case visit it with this visitor
        if(sub instanceof AbstractNode) {
            return visitAny((AbstractNode) sub, context);
        } else if (sub instanceof Condition<?>) {
            return ((Condition<?>) sub).query(this, context);
        } else {
            throw new IllegalArgumentException("Unknown node value type for subquery.");
        }

    }

    protected Object single(Collection<?> values) {
        if(!values.isEmpty()) {
            return values.iterator().next();
        } else {
            throw new IllegalArgumentException("You must provide a non-null query value for the condition.");
        }
    }


    public final T visitAny(AbstractNode node, S context) {

        // skip straight to the children if it's a logical node with one member
        if(node instanceof LogicalNode) {
            LogicalNode logical = (LogicalNode) node;
            if(logical.getChildren().size() == 1) {
                return visitAny(logical.getChildren().get(0), context);
            }
        }

        if(node instanceof AndNode){
            return visit((AndNode)node, context);
        } else if (node instanceof OrNode){
            return visit((OrNode)node, context);
        } else {
            return visit((ComparisonNode)node, context);
        }

    }

}
