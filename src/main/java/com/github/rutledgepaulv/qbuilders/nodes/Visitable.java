package com.github.rutledgepaulv.qbuilders.nodes;

import com.github.rutledgepaulv.qbuilders.visitors.ContextualNodeVisitor;

public interface Visitable {

    default <T> T visit(ContextualNodeVisitor<T,Void> visitor) {
        return visit(visitor, null);
    }

     <T,S> T visit(ContextualNodeVisitor<T,S> visitor, S context);

}
