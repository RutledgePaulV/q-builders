/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.nodes.OrNode
 *  *  *
 *  *  * Copyright (C) 2016 Paul Rutledge <paul.v.rutledge@gmail.com>
 *  *  *
 *  *  * This software may be modified and distributed under the terms
 *  *  * of the MIT license.  See the LICENSE file for details.
 *  *
 *
 */

package com.github.rutledgepaulv.qbuilders.nodes;

import java.util.List;

public final class OrNode extends LogicalNode {

    public OrNode() {}

    public OrNode(LogicalNode parent) {
        super(parent);
    }

    public OrNode(LogicalNode parent, List<AbstractNode> children) {
        super(parent, children);
    }


}
