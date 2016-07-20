/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.nodes.Visitable
 *  *  *
 *  *  * Copyright (C) 2016 Paul Rutledge <paul.v.rutledge@gmail.com>
 *  *  *
 *  *  * This software may be modified and distributed under the terms
 *  *  * of the MIT license.  See the LICENSE file for details.
 *  *
 *
 */

package com.github.rutledgepaulv.qbuilders.nodes;

import com.github.rutledgepaulv.qbuilders.visitors.ContextualNodeVisitor;

public interface Visitable {

    default <T> T visit(ContextualNodeVisitor<T,Void> visitor) {
        return visit(visitor, null);
    }

     <T,S> T visit(ContextualNodeVisitor<T,S> visitor, S context);

}
