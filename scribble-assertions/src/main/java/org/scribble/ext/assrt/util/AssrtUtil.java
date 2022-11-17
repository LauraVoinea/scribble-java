package org.scribble.ext.assrt.util;

import org.scribble.util.Pair;

public class AssrtUtil {

    public static String pairToString(Pair<?, ?> p) {
       return "(" + p.left + ", " + p.right + ")";
    }

    public static String tripleToString(Triple<?, ?, ?> p) {
        return "(" + p.left + ", " + p.middle + ", " + p.right + ")";
    }

    public static String quadpleToString(Quadple<?, ?, ?, ?> p) {
        return "(" + p.fst + ", " + p.snd + ", " + p.thd + ", " + p.fth + ")";
    }
}
