/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.visitors.AbstractVoidContextNodeVisitor
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

@SuppressWarnings("ConstantConditions")
public abstract class AbstractVoidContextNodeVisitor<T> extends ContextualNodeVisitor<T, Void> {

    @Override
    protected final T visit(AndNode node, Void context) {
        return visit(node);
    }

    @Override
    protected final T visit(OrNode node, Void context) {
        return visit(node);
    }

    @Override
    protected final T visit(ComparisonNode node, Void context) {
        return visit(node);
    }

    protected abstract T visit(AndNode node);

    protected abstract T visit(OrNode node);

    protected abstract T visit(ComparisonNode node);

    protected T condition(ComparisonNode node) {
        return condition(node, null);
    }

    public final T visitAny(AbstractNode node) {
        return visitAny(node, null);
    }

}
