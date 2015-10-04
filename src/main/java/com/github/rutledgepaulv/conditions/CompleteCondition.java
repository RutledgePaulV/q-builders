package com.github.rutledgepaulv.conditions;

import com.github.rutledgepaulv.visitors.NodeVisitor;

public interface CompleteCondition<T extends PartialCondition> {

    T and();

    T or();

    <Q> Q query(NodeVisitor<Q> visitor);
}
