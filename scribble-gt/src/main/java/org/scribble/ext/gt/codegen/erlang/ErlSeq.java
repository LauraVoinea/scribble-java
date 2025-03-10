package org.scribble.ext.gt.codegen.erlang;

import java.io.IOException;
import java.util.*;

/** Represents a sequence of Erlang expressions (used for function or case bodies). */
public class ErlSeq implements ErlTerm {
    private List<ErlTerm> expressions = new ArrayList<>();
    public ErlSeq() {}
    public ErlSeq(List<ErlTerm> exprs) {
        this.expressions.addAll(exprs);
    }
    public void addExpression(ErlTerm expr) {
        expressions.add(expr);
    }
    /** Get the list of expressions (for use by containing constructs). */
    public List<ErlTerm> getExpressions() {
        return expressions;
    }
    @Override
    public void write(FileWriter w) throws IOException {
        for (int i = 0; i < expressions.size(); ++i) {
            expressions.get(i).write(w);
            if (i < expressions.size() - 1) {
                w.write(",");
                w.writeLine("");
            } else {
                w.writeLine("");
            }
        }
    }
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < expressions.size(); ++i) {
            sb.append(expressions.get(i).toString());
            if (i < expressions.size() - 1) {
                sb.append(", ");
            }
        }
        return sb.toString();
    }
}
