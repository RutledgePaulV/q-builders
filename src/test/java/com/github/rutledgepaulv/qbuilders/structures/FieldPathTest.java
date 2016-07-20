/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.structures.FieldPathTest
 *  *  *
 *  *  * Copyright (C) 2016 Paul Rutledge <paul.v.rutledge@gmail.com>
 *  *  *
 *  *  * This software may be modified and distributed under the terms
 *  *  * of the MIT license.  See the LICENSE file for details.
 *  *
 *
 */

package com.github.rutledgepaulv.qbuilders.structures;

import org.junit.Test;

import java.util.List;

import static java.util.stream.Collectors.*;
import static org.junit.Assert.*;

public class FieldPathTest {

    private FieldPath it = new FieldPath("com.github.rutledgepaulv.myList");

    @Test
    public void iterator() throws Exception {
        List<String> segments = it.stream().flatMap(FieldPath.FieldNamespace::stream).collect(toList());
        assertEquals("com", segments.get(0));
        assertEquals("github", segments.get(1));
        assertEquals("rutledgepaulv", segments.get(2));
        assertEquals("myList", segments.get(3));
    }

    @Test
    public void append_String() throws Exception {
        FieldPath oneMore = it.append("name");
        assertEquals("com.github.rutledgepaulv.myList.name", oneMore.asFullyQualifiedKey());
        assertEquals("name", oneMore.asKey());

        FieldPath twoMore = it.append("name", "again");
        assertEquals("com.github.rutledgepaulv.myList.name.again", twoMore.asFullyQualifiedKey());
        assertEquals("again", twoMore.asKey());
    }

    @Test
    public void append_Path() throws Exception {
        FieldPath oneMore = it.append(new FieldPath("name"));
        assertEquals("com.github.rutledgepaulv.myList.name", oneMore.asFullyQualifiedKey());
        assertEquals("name", oneMore.asKey());
    }

    @Test
    public void prepend_String() throws Exception {
        FieldPath oneMore = it.prepend("name");
        assertEquals("name.com.github.rutledgepaulv.myList", oneMore.asFullyQualifiedKey());
        assertEquals("com.github.rutledgepaulv.myList", oneMore.asKey());
    }

    @Test
    public void prepend_Path() throws Exception {
        FieldPath oneMore = it.prepend(new FieldPath("name"));
        assertEquals("name.com.github.rutledgepaulv.myList", oneMore.asFullyQualifiedKey());
        assertEquals("com.github.rutledgepaulv.myList", oneMore.asKey());
    }

    @Test
    public void asFullyQualifiedPrefix() throws Exception {
        FieldPath oneMore = it.append("name");
        assertEquals("com.github.rutledgepaulv.myList.", it.asFullyQualifiedPrefix());
        assertEquals("com.github.rutledgepaulv.myList.name.", oneMore.asFullyQualifiedPrefix());
    }

    @Test
    public void asFullyQualifiedKey() throws Exception {
        FieldPath oneMore = it.append("name");
        assertEquals("com.github.rutledgepaulv.myList", it.asFullyQualifiedKey());
        assertEquals("com.github.rutledgepaulv.myList.name", oneMore.asFullyQualifiedKey());
    }

    @Test
    public void asShortKey() throws Exception {
        FieldPath oneMore = it.append("name");
        assertEquals("name", oneMore.asKey());
    }

    @Test
    public void asShortPrefix() throws Exception {
        FieldPath oneMore = it.append("name");
        assertEquals("name.", oneMore.asPrefix());
    }

    @Test
    public void test_toString() throws Exception {
        FieldPath oneMore = it.append("name");
        assertEquals("com.github.rutledgepaulv.myList.name", oneMore.toString());
    }

    @Test
    public void testEquals() {
        FieldPath path = new FieldPath("test");
        assertTrue(path.equals(path));
        assertFalse(path.equals("cats"));
        assertFalse(path.equals(new FieldPath("bats")));
        assertTrue(path.equals(new FieldPath("test")));
    }

    @Test
    public void testHashCode() {
        FieldPath path = new FieldPath("test");
        assertEquals(path.hashCode(), path.hashCode());
        assertNotEquals(path.hashCode(), new FieldPath("badgers").hashCode());
    }


    @Test
    public void testGetParentPath() {
        FieldPath path = it.append("name");
        assertTrue(path.getParentPath().isPresent());
        assertEquals("com.github.rutledgepaulv.myList", path.getParentPath().get().asFullyQualifiedKey());
    }

}
