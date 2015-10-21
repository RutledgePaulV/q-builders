package com.github.rutledgepaulv.qbuilders.utilities;

import java.util.Arrays;
import java.util.Iterator;
import java.util.stream.Stream;

public final class PathUtils {
    private PathUtils() {
    }

    public static Stream<String> stream(String path) {
        return Arrays.stream(path.split("\\."));
    }

    public static Iterator<String> iterator(String path) {
        return stream(path).iterator();
    }

}