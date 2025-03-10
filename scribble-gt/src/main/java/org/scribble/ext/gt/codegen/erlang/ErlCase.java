package org.scribble.ext.gt.codegen.erlang;

import java.io.IOException;
import java.util.*;

/** Represents an Erlang case expression. */
public class ErlCase implements ErlTerm {
    private ErlTerm expr;
    private List<CaseClause> clauses = new ArrayList<>();
    /** Inner class to represent a case clause (pattern, guard, body). */
    private static class CaseClause {
        ErlTerm pattern;
        ErlGuard guard;
        ErlTerm body;
        CaseClause(ErlTerm pattern, ErlGuard guard, ErlTerm body) {
            this.pattern = pattern;
            this.guard = guard;
            this.body = body;
        }
    }
    public ErlCase(ErlTerm expr) {
        this.expr = expr;
    }
    public void addClause(ErlTerm pattern, ErlGuard guard, ErlTerm body) {
        clauses.add(new CaseClause(pattern, guard, body));
    }
    public void addClause(ErlTerm pattern, ErlTerm body) {
        clauses.add(new CaseClause(pattern, null, body));
    }
    @Override
    public void write(FileWriter w) throws IOException {
        w.writeLine("case " + expr.toString() + " of");
        w.indent();
        for (int i = 0; i < clauses.size(); ++i) {
            CaseClause cc = clauses.get(i);
            // Write pattern and guard
            cc.pattern.write(w);
            if (cc.guard != null && !cc.guard.toString().isEmpty()) {
                w.write(" when ");
                cc.guard.write(w);
            }
            w.writeLine(" ->");
            // Write clause body
            w.indent();
            List<ErlTerm> bodyExprs;
            if (cc.body instanceof ErlSeq) {
                bodyExprs = ((ErlSeq) cc.body).getExpressions();
            } else {
                bodyExprs = Arrays.asList(cc.body);
            }
            for (int j = 0; j < bodyExprs.size(); ++j) {
                bodyExprs.get(j).write(w);
                if (j < bodyExprs.size() - 1) {
                    w.write(",");
                    w.writeLine("");
                } else {
                    // last expression in clause body
                }
            }
            w.dedent();
            if (i < clauses.size() - 1) {
                w.write(";");
                w.writeLine("");
            } else {
                w.writeLine("");
            }
        }
        w.dedent();
        w.write("end");
    }
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("case ").append(expr.toString()).append(" of ");
        for (int i = 0; i < clauses.size(); ++i) {
            CaseClause cc = clauses.get(i);
            sb.append(cc.pattern.toString());
            if (cc.guard != null && !cc.guard.toString().isEmpty()) {
                sb.append(" when ").append(cc.guard.toString());
            }
            sb.append(" -> ").append(cc.body.toString());
            if (i < clauses.size() - 1) {
                sb.append("; ");
            }
        }
        sb.append(" end");
        return sb.toString();
    }
}
