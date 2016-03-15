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
    public void append() throws Exception {
        FieldPath oneMore = it.append("name");
        assertEquals("com.github.rutledgepaulv.myList.name", oneMore.asFullyQualifiedKey());
        assertEquals("name", oneMore.asKey());

        FieldPath twoMore = it.append("name", "again");
        assertEquals("com.github.rutledgepaulv.myList.name.again", twoMore.asFullyQualifiedKey());
        assertEquals("again", twoMore.asKey());
    }

    @Test
    public void append1() throws Exception {

    }

    @Test
    public void prepend() throws Exception {

    }

    @Test
    public void prepend1() throws Exception {

    }

    @Test
    public void asFullyQualifiedPrefix() throws Exception {

    }

    @Test
    public void asFullyQualifiedKey() throws Exception {

    }

    @Test
    public void asShortKey() throws Exception {

    }

    @Test
    public void test_toString() throws Exception {

    }

}
