package com.github.rutledgepaulv.basic.qbuilders.conditions;

import com.github.rutledgepaulv.basic.qbuilders.visitors.NodeVisitor;

public interface CompleteCondition<T extends PartialCondition> {

    T and();

    T or();

    <Q> Q query(NodeVisitor<Q> visitor);
}
