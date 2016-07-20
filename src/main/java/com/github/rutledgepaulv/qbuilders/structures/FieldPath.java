/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.structures.FieldPath
 *  *  *
 *  *  * Copyright (C) 2016 Paul Rutledge <paul.v.rutledge@gmail.com>
 *  *  *
 *  *  * This software may be modified and distributed under the terms
 *  *  * of the MIT license.  See the LICENSE file for details.
 *  *
 *
 */

package com.github.rutledgepaulv.qbuilders.structures;

import org.apache.commons.lang3.StringUtils;

import java.util.*;
import java.util.stream.*;

import static java.util.stream.Collectors.*;

public class FieldPath implements Iterable<FieldPath.FieldNamespace> {

    private List<FieldNamespace> chain = new LinkedList<>();

    public FieldPath(String raw) {
        chain.add(new FieldNamespace(raw));
    }

    public FieldPath(List<FieldNamespace> namespaces) {
        chain.addAll(namespaces);
    }

    @Override
    public Iterator<FieldNamespace> iterator() {
        return chain.iterator();
    }

    public Stream<FieldNamespace> stream() {
        return streamIter(this);
    }

    public Optional<FieldPath> getParentPath() {
        if (chain.size() > 1) {
            return Optional.of(new FieldPath(chain.subList(0, chain.size() - 1)));
        } else {
            return Optional.empty();
        }
    }

    public FieldPath append(String... path) {
        List<FieldNamespace> chain = new LinkedList<>();
        chain.addAll(this.chain);
        chain.addAll(Arrays.stream(path).map(FieldNamespace::new).collect(Collectors.toList()));
        return new FieldPath(chain);
    }

    public FieldPath append(FieldPath... path) {
        List<FieldNamespace> newChain = new LinkedList<>();
        newChain.addAll(this.chain);
        newChain.addAll(Arrays.stream(path).flatMap($ ->
                $.chain.stream()).collect(Collectors.toList()));
        return new FieldPath(newChain);
    }

    public FieldPath prepend(String path) {
        List<FieldNamespace> chain = new LinkedList<>();
        chain.add(new FieldNamespace(path));
        chain.addAll(this.chain);
        return new FieldPath(chain);
    }

    public FieldPath prepend(FieldPath path) {
        List<FieldNamespace> chain = new LinkedList<>();
        chain.addAll(path.chain);
        chain.addAll(this.chain);
        return new FieldPath(chain);
    }

    public String asFullyQualifiedKey() {
        return stream().map(Objects::toString).collect(joining("."));
    }

    public String asFullyQualifiedPrefix() {
        return stream().map(Objects::toString).collect(joining(".", "", "."));
    }

    public String asKey() {
        return chain.get(chain.size() - 1).toString();
    }

    public String asPrefix() {
        return chain.get(chain.size() - 1).toString() + ".";
    }

    @Override
    public String toString() {
        return asFullyQualifiedKey();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof FieldPath)) {
            return false;
        }
        FieldPath that = (FieldPath) o;
        return Objects.equals(chain, that.chain);
    }

    @Override
    public int hashCode() {
        return Objects.hash(chain);
    }

    public static class FieldNamespace implements Iterable<String> {
        private String raw;

        private FieldNamespace(String raw) {
            this.raw = strip(raw);
        }

        @Override
        public Iterator<String> iterator() {
            return Arrays.stream(StringUtils.split(raw, ".")).iterator();
        }

        public Stream<String> stream() {
            return StreamSupport.stream(spliterator(), false);
        }

        @Override
        public String toString() {
            return raw;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (!(o instanceof FieldNamespace)) {
                return false;
            }
            FieldNamespace strings = (FieldNamespace) o;
            return Objects.equals(raw, strings.raw);
        }

        @Override
        public int hashCode() {
            return Objects.hash(raw);
        }
    }

    private static String strip(String value) {
        return StringUtils.strip(value, ".");
    }

    private static <T> Stream<T> streamIter(Iterable<T> iterable) {
        return StreamSupport.stream(iterable.spliterator(), false);
    }

}
