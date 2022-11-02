package org.scribble.ext.assrt.util;

import org.scribble.util.Pair;

public class AssrtUtil {

    public static String pairToString(Pair<?, ?> p) {
       return "(" + p.left + ", " + p.right + ")";
    }
}
