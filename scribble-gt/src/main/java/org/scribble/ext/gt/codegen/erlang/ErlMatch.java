package org.scribble.ext.gt.codegen.erlang;

import java.io.IOException;

/** Represents a pattern match (Pattern = Expression). */
public class ErlMatch implements ErlTerm {
    private ErlTerm left;
    private ErlTerm right;
    public ErlMatch(ErlTerm left, ErlTerm right) {
        this.left = left;
        this.right = right;
    }
    @Override
    public void write(FileWriter w) throws IOException {
        left.write(w);
        w.write(" = ");
        right.write(w);
    }
    @Override
    public String toString() {
        return left.toString() + " = " + right.toString();
    }
}

