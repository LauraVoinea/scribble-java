package org.scribble.ext.assrt.util;

import org.scribble.util.Pair;

public class AssrtUtil {

    private static String pairToString1(Object p) {
        return p instanceof Pair<?, ?>
                ? pairToString((Pair<?, ?>) p)
                : p.toString();
    }

    public static String pairToString(Pair<?, ?> p) {
        return "(" + pairToString1(p.left) + ", " + pairToString1(p.right) + ")";
    }

    public static String tripleToString(Triple<?, ?, ?> p) {
        return "(" + p.left + ", " + p.middle + ", " + p.right + ")";
    }

    public static String quadpleToString(Quadple<?, ?, ?, ?> p) {
        return "(" + p.fst + ", " + p.snd + ", " + p.thd + ", " + p.fth + ")";
    }
}
