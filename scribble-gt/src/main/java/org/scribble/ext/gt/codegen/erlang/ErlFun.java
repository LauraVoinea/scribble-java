package org.scribble.ext.gt.codegen.erlang;

//import java.util.ArrayList;
//import java.util.Collections;
//import java.util.List;
//import java.util.Optional;
//import java.util.stream.Collectors;
import java.io.IOException;
import java.util.*;

public class ErlFun implements ErlTerm {
    private String name;
    /** Internal representation of a function clause: arguments, guard, and body. */
    private static class FunClause {
        List<ErlTerm> args;
        ErlGuard guard;
        ErlTerm body;
        FunClause(List<ErlTerm> args, ErlGuard guard, ErlTerm body) {
            this.args = args;
            this.guard = guard;
            this.body = body;
        }
    }
    private List<FunClause> clauses = new ArrayList<>();

    public ErlFun(String name) {
        this.name = name;
    }

    /** Add a function clause with given argument patterns, guard (optional), and body. */
    public void addClause(List<ErlTerm> args, ErlGuard guard, ErlTerm body) {
        clauses.add(new FunClause(new ArrayList<>(args), guard, body));
    }
    /** Convenience: add a clause without a guard. */
    public void addClause(List<ErlTerm> args, ErlTerm body) {
        clauses.add(new FunClause(new ArrayList<>(args), null, body));
    }

    @Override
    public void write(FileWriter w) throws IOException {
        for (int i = 0; i < clauses.size(); ++i) {
            FunClause cl = clauses.get(i);
            // Write function head
            w.write(name + "(");
            for (int j = 0; j < cl.args.size(); ++j) {
                cl.args.get(j).write(w);
                if (j < cl.args.size() - 1) {
                    w.write(", ");
                }
            }
            w.write(")");
            if (cl.guard != null && !cl.guard.toString().isEmpty()) {
                w.write(" when ");
                cl.guard.write(w);
            }
            w.writeLine(" ->");
            // Write function body
            w.indent();
            // If body is a sequence, get each expression; otherwise single expression
            List<ErlTerm> bodyExprs;
            if (cl.body instanceof ErlSeq) {
                bodyExprs = ((ErlSeq) cl.body).getExpressions();
            } else {
                bodyExprs = Arrays.asList(cl.body);
            }
            for (int k = 0; k < bodyExprs.size(); ++k) {
                bodyExprs.get(k).write(w);
                if (k < bodyExprs.size() - 1) {
                    // not last expression in body -> add comma and newline
                    w.write(",");
                    w.writeLine("");
                } else {
                    // last expression in body: do not newline yet (handle after dedent)
                }
            }
            w.dedent();
            // Add clause terminator (; or .)
            if (i < clauses.size() - 1) {
                w.write(";");
                w.writeLine("");
            } else {
                w.write(".");
                w.writeLine("");
            }
        }
    }

    @Override
    public String toString() {
        // Quick representation: not fully pretty, but for debugging
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < clauses.size(); ++i) {
            FunClause cl = clauses.get(i);
            sb.append(name).append("(");
            for (int j = 0; j < cl.args.size(); ++j) {
                sb.append(cl.args.get(j).toString());
                if (j < cl.args.size() - 1) sb.append(", ");
            }
            sb.append(")");
            if (cl.guard != null && !cl.guard.toString().isEmpty()) {
                sb.append(" when ").append(cl.guard.toString());
            }
            sb.append(" -> ").append(cl.body.toString());
            sb.append((i < clauses.size() - 1) ? "; " : ".");
        }
        return sb.toString();
    }
}