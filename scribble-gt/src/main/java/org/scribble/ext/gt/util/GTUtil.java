package org.scribble.ext.gt.util;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;

public class GTUtil {

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

    public static <T> LinkedHashSet<T> setOf() {
        return new LinkedHashSet<>();
    }

    public static <T> LinkedHashSet<T> setOf(T... ts) {
        LinkedHashSet<T> res = new LinkedHashSet<>();
        res.addAll(Arrays.asList(ts));
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
}
