package com.github.rutledgepaulv.qbuilders.visitors;


import com.github.rutledgepaulv.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.qbuilders.nodes.*;
import com.github.rutledgepaulv.qbuilders.operators.ComparisonOperator;

@SuppressWarnings("ConstantConditions")
public abstract class NodeVisitor<T> {

    protected abstract T visit(AndNode node);

    protected abstract T visit(OrNode node);

    protected abstract T visit(ComparisonNode node);

    /**
     * Build a comparison node value into a visited value so that
     * it can be composed into the larger query being built.
     *
     * @param node The node with a condition argument to build into a visited value.
     *
     * @return The visited value.
     */
    protected final T condition(ComparisonNode node) {
        if(!node.getOperator().equals(ComparisonOperator.SUB_CONDITION_ANY)) {
            throw new IllegalArgumentException("You can only build a condition for sub-condition operator nodes.");
        }

        Condition<?> condition = (Condition<?>) node.getValues().iterator().next();

        return condition.query(this);
    }


    public final T visitAny(AbstractNode node) {

        // skip straight to the children if it's a logical node with one member
        if(node instanceof LogicalNode && node.getChildren().size() == 1) {
            return visitAny(node.getChildren().get(0));
        }

        if(node instanceof AndNode){
            return visit((AndNode)node);
        } else if (node instanceof OrNode){
            return visit((OrNode)node);
        } else {
            return visit((ComparisonNode)node);
        }

    }

}
