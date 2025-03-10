package org.scribble.ext.gt.codegen.erlang;

import java.io.IOException;

public class ErlOp implements ErlTerm {
    private final String op;

    public ErlOp(String op) {
        this.op = op;
    }

    @Override
    public void write(FileWriter w) throws IOException {
        w.write(op);
    }

    @Override
    public String toString() {
        return op;
    }
}
