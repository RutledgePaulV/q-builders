package com.github.rutledgepaulv.qbuilders.structures;

import org.apache.commons.lang3.StringUtils;

import java.util.*;
import java.util.stream.*;

import static java.util.stream.Collectors.*;
import static java.util.stream.Stream.*;

public class FieldPath implements Iterable<FieldPath.FieldNamespace> {

    private List<FieldNamespace> chain = new LinkedList<>();

    public FieldPath(String raw) {
        chain.add(new FieldNamespace(raw));
    }

    public FieldPath(FieldNamespace namespace) {
        chain.add(namespace);
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

    public FieldPath append(FieldNamespace... path) {
        List<FieldNamespace> chain = new LinkedList<>();
        chain.addAll(this.chain);
        chain.addAll(Arrays.stream(path).collect(Collectors.toList()));
        return new FieldPath(chain);
    }

    public FieldPath append(FieldPath... path) {
        List<FieldNamespace> chain = Arrays.stream(path).flatMap($ -> $.chain.stream()).collect(Collectors.toList());
        return new FieldPath(chain);
    }

    public FieldPath prepend(String path) {
        List<FieldNamespace> chain = new LinkedList<>();
        chain.add(new FieldNamespace(path));
        chain.addAll(this.chain);
        return new FieldPath(chain);
    }

    public FieldPath prepend(FieldNamespace path) {
        List<FieldNamespace> chain = new LinkedList<>();
        chain.add(path);
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

        public FieldNamespace append(String... path) {
            return new FieldNamespace(
                    concat(Stream.of(raw), Arrays.stream(path)).map(FieldPath::strip).collect(joining(".")));
        }

        public FieldNamespace append(FieldNamespace... path) {
            return append(Arrays.stream(path).map($ -> $.raw).toArray(String[]::new));
        }

        public FieldNamespace prepend(String path) {
            return new FieldNamespace(StringUtils.join(new String[]{path, raw}, "."));
        }

        public FieldNamespace prepend(FieldNamespace path) {
            return prepend(path.raw);
        }

        @Override
        public String toString() {
            return raw;
        }
    }

    private static String strip(String value) {
        return StringUtils.strip(value, ".");
    }

    private static <T> Stream<T> streamIter(Iterable<T> iterable) {
        return StreamSupport.stream(iterable.spliterator(), false);
    }

}
