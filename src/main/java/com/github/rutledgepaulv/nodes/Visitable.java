package com.github.rutledgepaulv.nodes;

import com.github.rutledgepaulv.visitors.NodeVisitor;

public interface Visitable {

    <T> T visit(NodeVisitor<T> visitor);

}
