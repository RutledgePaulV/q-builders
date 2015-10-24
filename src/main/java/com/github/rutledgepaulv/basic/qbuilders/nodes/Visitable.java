package com.github.rutledgepaulv.basic.qbuilders.nodes;

import com.github.rutledgepaulv.basic.qbuilders.visitors.NodeVisitor;

public interface Visitable {

    <T> T visit(NodeVisitor<T> visitor);

}
