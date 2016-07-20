/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.properties.virtual.Property
 *  *  *
 *  *  * Copyright (C) 2016 Paul Rutledge <paul.v.rutledge@gmail.com>
 *  *  *
 *  *  * This software may be modified and distributed under the terms
 *  *  * of the MIT license.  See the LICENSE file for details.
 *  *
 *
 */

package com.github.rutledgepaulv.qbuilders.properties.virtual;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;

/**
 * A marker interface for properties that have been defined. Properties
 * represent queryable field types and expose mechanisms to define the
 * constraints against hte field for the query.
 *
 * @param <T> The final type of the builder.
 */
@SuppressWarnings("unused")
public interface Property<T extends QBuilder<T>> {
}
