package com.github.rutledgepaulv.basic.qbuilders.conditions;

import com.github.rutledgepaulv.basic.qbuilders.visitors.NodeVisitor;

public interface Condition<T extends Partial> {

    T and();

    T or();

    <Q> Q query(NodeVisitor<Q> visitor);
}
