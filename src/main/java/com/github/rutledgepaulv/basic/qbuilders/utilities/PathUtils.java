package com.github.rutledgepaulv.basic.qbuilders.utilities;

import java.util.Arrays;
import java.util.Iterator;
import java.util.stream.Stream;

public abstract class PathUtils {

    public static Stream<String> stream(String path) {
        return Arrays.stream(path.split("\\."));
    }

    public static Iterator<String> iterator(String path) {
        return stream(path).iterator();
    }

}