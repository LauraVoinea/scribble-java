package org.scribble.ext.ea.util;

import org.scribble.util.Pair;

public class EAPPair<L, R> extends Pair<L, R> {

    public EAPPair(L left, R right) {
        super(left, right);
    }

    @Override
    public String toString() {
        return "(" + this.left + ", " + this.right + ")";
    }
}
