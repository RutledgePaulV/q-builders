package com.github.rutledgepaulv.qbuilders.nodes;

import com.github.rutledgepaulv.qbuilders.visitors.NodeVisitor;

public interface Visitable {

    <T> T visit(NodeVisitor<T> visitor);

}
