package com.github.rutledgepaulv.qbuilders.conditions;

import com.github.rutledgepaulv.qbuilders.visitors.NodeVisitor;

public interface CompleteCondition<T extends PartialCondition> {

    T and();

    T or();

    <Q> Q query(NodeVisitor<Q> visitor);
}
