package org.scribble.ext.gt.codegen.erlang;

import java.io.IOException;
import java.util.*;

/** Represents a call to an Erlang function (local or remote). */
public class ErlCall implements ErlTerm {
    private ErlTerm module;   // optional module name for remote call
    private ErlTerm function; // function name (or fun variable)
    private List<ErlTerm> args;

    /** Construct a local function call: functionName(Args...). */
    public ErlCall(String functionName, List<ErlTerm> args) {
        this.module = null;
        this.function = new ErlAtom(functionName);
        this.args = new ArrayList<>(args);
    }
    /** Construct a remote function call: module:function(Args...). */
    public ErlCall(String moduleName, String functionName, List<ErlTerm> args) {
        this.module = (moduleName != null ? new ErlAtom(moduleName) : null);
        this.function = new ErlAtom(functionName);
        this.args = new ArrayList<>(args);
    }
    /** Construct a call with an operator or special function (op symbol as function name). */
    public ErlCall(ErlOp opSymbol, List<ErlTerm> args) {
        this.module = null;
        this.function = opSymbol;
        this.args = new ArrayList<>(args);
    }

    /** Construct a remote function call: module:function(Args...). */
    public ErlCall(ErlTerm module, String functionName, List<ErlTerm> args) {
        this.module = module;
        this.function = new ErlAtom(functionName);
        this.args = new ArrayList<>(args);
    }

    @Override
    public void write(FileWriter w) throws IOException {
        if (function instanceof ErlOp) {
            // Write in infix style:
            // Assume there are exactly two arguments for an operator.
            if (args.size() == 2) {
                args.get(0).write(w);
                w.write(" " + function.toString() + " ");
                args.get(1).write(w);
            } else {
                // Fallback: write as a normal call
                function.write(w);
                w.write("(");
                for (int i = 0; i < args.size(); ++i) {
                    args.get(i).write(w);
                    if (i < args.size() - 1) {
                        w.write(", ");
                    }
                }
                w.write(")");
            }
        } else {
            if (module != null) {
                module.write(w);
                w.write(":");
            }
            function.write(w);
            w.write("(");
            for (int i = 0; i < args.size(); ++i) {
                args.get(i).write(w);
                if (i < args.size() - 1) {
                    w.write(", ");
                }
            }
            w.write(")");
        }
    }


    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (module != null) {
            sb.append(module.toString()).append(":");
        }
        sb.append(function.toString()).append("(");
        for (int i = 0; i < args.size(); ++i) {
            sb.append(args.get(i).toString());
            if (i < args.size() - 1) {
                sb.append(", ");
            }
        }
        sb.append(")");
        return sb.toString();
    }
}
