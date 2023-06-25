package org.scribble.ext.ea.util;

import java.util.*;
import java.util.stream.Collectors;

public class EAUtil {

    /* ... */

    public static <K, V> LinkedHashMap<K, V> mapOf() {
        return new LinkedHashMap<>();
    }

    public static <K, V> LinkedHashMap<K, V> mapOf(K k1, V v1) {
        LinkedHashMap<K, V> res = new LinkedHashMap<>();
        res.put(k1, v1);
        return res;
    }

    public static <K, V> LinkedHashMap<K, V> mapOf(K k1, V v1, K k2, V v2) {
        LinkedHashMap<K, V> res = new LinkedHashMap<>();
        res.put(k1, v1);
        res.put(k2, v2);
        return res;
    }

    public static <K, V> LinkedHashMap<K, V> mapOf(K k1, V v1, K k2, V v2, K k3, V v3) {
        LinkedHashMap<K, V> res = new LinkedHashMap<>();
        res.put(k1, v1);
        res.put(k2, v2);
        res.put(k3, v3);
        return res;
    }

    public static <K, V> LinkedHashMap<K, V> mapOf(Map<K, V> m, K k1, V v1) {
        LinkedHashMap<K, V> res = new LinkedHashMap<>(m);
        res.put(k1, v1);
        return res;
    }

    public static <K, V> LinkedHashMap<K, V> mapOf(Map<K, V> m, K k1, V v1, K k2, V v2) {
        LinkedHashMap<K, V> res = new LinkedHashMap<>(m);
        res.put(k1, v1);
        res.put(k2, v2);
        return res;
    }

    public static <T> LinkedHashSet<T> setOf() {
        return new LinkedHashSet<>();
    }

    public static <T> LinkedHashSet<T> setOf(T... ts) {
        LinkedHashSet<T> res = new LinkedHashSet<>();
        res.addAll(Arrays.asList(ts));
        return res;
    }

    public static <T> LinkedHashSet<T> setOf(Set<T> ts, T... add) {
        LinkedHashSet<T> res = new LinkedHashSet<>(ts);
        res.addAll(Arrays.asList(add));
        return res;
    }

    public static <T> LinkedList<T> listOf() {
        return new LinkedList<>();
    }

    public static <T> LinkedList<T> listOf(T... ts) {
        LinkedList<T> res = new LinkedList<>();
        res.addAll(Arrays.asList(ts));
        return res;
    }

    public static <T> LinkedList<T> listOf(List<T> ts, T... app) {
        LinkedList<T> res = new LinkedList<>(ts);
        res.addAll(Arrays.asList(app));
        return res;
    }

    /* ... */

    public static <T> LinkedList<T> copyOf(List<T> c) {
        return new LinkedList<>(c);
    }

    public static <T> LinkedHashSet<T> copyOf(Set<T> c) {
        return new LinkedHashSet<>(c);
    }

    public static <K, V> LinkedHashMap<K, V> copyOf(Map<K, V> c) {
        return new LinkedHashMap<>(c);
    }

    /* ... */

    public static <K, V> LinkedHashMap<K, List<V>> copyOfList(Map<K, List<V>> m) {
        return m.entrySet().stream().collect(Collectors.toMap(
                Map.Entry::getKey,
                x -> copyOf(x.getValue()),
                (x, y) -> {
                    throw new RuntimeException("Key conflict: " + x + ", " + y);
                },
                LinkedHashMap::new
        ));
    }

    public static <K1, K2, V> LinkedHashMap<K1, Map<K2, V>> copyOfMap(Map<K1, Map<K2, V>> m) {
        return m.entrySet().stream().collect(Collectors.toMap(
                Map.Entry::getKey,
                x -> copyOf(x.getValue()),
                (x, y) -> {
                    throw new RuntimeException("Key conflict: " + x + ", " + y);
                },
                LinkedHashMap::new
        ));
    }

    /* ... */

    public static <T> List<T> umod(List<T> c) {
        return Collections.unmodifiableList(c);
    }

    public static <T> Set<T> umod(Set<T> c) {
        return Collections.unmodifiableSet(c);
    }

    public static <K, V> Map<K, V> umod(Map<K, V> m) {
        return Collections.unmodifiableMap(m);
    }

    public static <T> List<T> umodListOf() {
        return umod(listOf());
    }

    public static <T> Set<T> umodSetOf() {
        return umod(setOf());
    }

    public static <K, V> Map<K, V> umodMapOf() {
        return umod(mapOf());
    }

    /* ... */

    public static <T> List<T> umodCopyOf(List<T> c) {
        return umod(copyOf(c));
    }

    public static <T> Set<T> umodCopyOf(Set<T> c) {
        return umod(copyOf(c));
    }

    public static <K, V> Map<K, V> umodCopyOf(Map<K, V> m) {
        return umod(copyOf(m));
    }

    /* ... */

    // N.B. does *nested* umod copy
    public static <K, V> Map<K, List<V>> umodCopyOfList(Map<K, List<V>> m) {
        return umod(m.entrySet().stream().collect(Collectors.toMap(
                Map.Entry::getKey,
                x -> umodCopyOf(x.getValue())  // FIXME key clash, cf. copyOfList
        )));
    }

    // N.B. does *nested* umod copy
    public static <K1, K2, V> Map<K1, Map<K2, V>> umodCopyOfMap(Map<K1, Map<K2, V>> m) {
        return umod(m.entrySet().stream().collect(Collectors.toMap(
                Map.Entry::getKey,
                x -> umodCopyOf(x.getValue())  // FIXME key clash, cf. copyOfList
        )));
    }
}
