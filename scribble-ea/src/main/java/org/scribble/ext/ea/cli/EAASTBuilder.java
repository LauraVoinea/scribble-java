package org.scribble.ext.ea.cli;


import org.antlr.runtime.tree.CommonTree;
import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.config.EAPRuntimeFactory;
import org.scribble.ext.ea.core.process.*;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.session.local.*;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.util.EAPPair;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.stream.Collectors;

// Move to parsing
class EAASTBuilder {
    static EAPFactory pf = EAPFactory.factory;
    static EAPRuntimeFactory rf = EAPRuntimeFactory.factory;
    static EATypeFactory tf = EATypeFactory.factory;

    /*public EAPTerm visit(CommonTree n) {  // Entry
        return visitM(n);
    }*/

    /* M */

    public EAPExpr visitM(CommonTree n) {
        switch (n.getText()) {
            case "M_LET":
                return visitLet(n);
            case "M_SEND":
                return visitSend(n);
            case "M_SUSPEND":
                return visitSuspend(n);
            case "M_RETURN":
                return visitReturn(n);
            case "M_APP":
                return visitApp(n);
        }
        throw new RuntimeException("Unknown node kind: " + n.getText());
    }

    public EAPLet visitLet(CommonTree n) {
        EAPVar var = visitVar((CommonTree) n.getChild(0));
        EAValType varType = visitA((CommonTree) n.getChild(1));
        EAPExpr e1 = visitM((CommonTree) n.getChild(2));
        EAPExpr e2 = visitM((CommonTree) n.getChild(3));
        return pf.let(var, varType, e1, e2);
    }

    public EAPSend visitSend(CommonTree n) {
        Role dst = visitRole((CommonTree) n.getChild(0));
        Op op = visitOp((CommonTree) n.getChild(1));
        EAPVal V = visitV((CommonTree) n.getChild(2));
        return pf.send(dst, op, V);
    }

    public EAPSuspend visitSuspend(CommonTree n) {
        EAPVal V = visitV((CommonTree) n.getChild(0));
        return pf.suspend(V);
    }

    public EAPReturn visitReturn(CommonTree n) {
        EAPVal V = visitV((CommonTree) n.getChild(0));
        return pf.returnn(V);
    }

    public EAPApp visitApp(CommonTree n) {
        EAPVal left = visitV((CommonTree) n.getChild(0));
        EAPVal right = visitV((CommonTree) n.getChild(1));
        return pf.app(left, right);
    }

    /* V */

    public EAPVal visitV(CommonTree n) {
        String txt = n.getText();
        switch (txt) {
            case "V_HANDLERS": {
                List<CommonTree> cs = ((List<Object>) n.getChildren()).stream()
                        .map(x -> (CommonTree) x).collect(Collectors.toList());
                Role r = visitRole(cs.get(0));
                LinkedHashMap<Op, EAPHandler> hs = visitHandlers(cs.subList(1, cs.size()));
                return pf.handlers(r, hs);
            }
            case "V_INT":
                return visitInt(n);
            case "V_VAR":
                return visitVar(n);
            case "V_UNIT":
                return pf.unit();
            case "V_REC": {
                EAPFuncName f = visitfname((CommonTree) n.getChild(0));
                EAPVar var = visitVar((CommonTree) n.getChild(1));
                EAValType varType = visitA((CommonTree) n.getChild(2));
                EALType S = visitSessionType((CommonTree) n.getChild(3));
                EALType T = visitSessionType((CommonTree) n.getChild(4));
                EAValType B = visitA((CommonTree) n.getChild(5));
                EAPExpr body = visitM((CommonTree) n.getChild(6));
                return pf.rec(f, var, varType, body, S, T, B);
            }
            default:
                throw new RuntimeException("Unknown V: " + n);
        }
    }

    public EAPVar visitVar(CommonTree n) {
        //return pf.var(n.getText());
        return pf.var(n.getChild(0).getText());
    }

    public EAPIntVal visitInt(CommonTree n) {
        String txt = n.getChild(0).getText();
        if (txt.equals("UNIT_KW")) {  // FIXME
            return pf.intt(1);
        }
        return pf.intt(Integer.parseInt(txt));
    }

    public LinkedHashMap<Op, EAPHandler> visitHandlers(List<CommonTree> n) {
        return n.stream().map(x -> visitHandler(x)).collect(Collectors.toMap(
                x -> x.op,
                x -> x,
                (x, y) -> null,
                LinkedHashMap::new
        ));
    }

    public EAPHandler visitHandler(CommonTree n) {
        Op op = visitOp((CommonTree) n.getChild(0));
        EAPVar var = visitVar((CommonTree) n.getChild(1));
        EAValType varType = visitA((CommonTree) n.getChild(2));
        EALType stype = visitSessionType((CommonTree) n.getChild(3));
        EAPExpr expr = visitM((CommonTree) n.getChild(4));
        return pf.handler(op, var, varType, expr, stype);
    }

    /* A */

    public EAValType visitA(CommonTree n) {
        String txt = n.getText();
        switch (txt) {
            case "A_HANDLER": {
                return tf.val.handlers((EALInType) visitSessionType((CommonTree) n.getChild(0)));
            }
            case "A_UNIT": {
                return tf.val.unit();
            }
            case "A_FUN": {
                EAValType A = visitA((CommonTree) n.getChild(0));
                EALType S = visitSessionType((CommonTree) n.getChild(1));
                EALType T = visitSessionType((CommonTree) n.getChild(2));
                EAValType B = visitA((CommonTree) n.getChild(3));
                return tf.val.func(A, S, T, B);
            }
            case "A_INT": {
                return tf.val.intt();
            }
            default:
                throw new RuntimeException("Unknown val type: " + n);
        }
    }

    /* S */

    public EALType visitSessionType(CommonTree n) {
        String txt = n.getText();
        switch (txt) {
            case "S_END":
                return tf.local.end();
            case "S_BRANCH":
                return visitBranch(n);
            case "S_SELECT":
                return visitSelect(n);
            case "S_REC":
                return visitRecursion(n);
            case "S_RECVAR":
                return visitRecVarType(n);
            default:
                throw new RuntimeException("Unknown session type: " + n);
        }
    }

    public EALInType visitBranch(CommonTree n) {
        Role r = visitRole((CommonTree) n.getChild(0));
        LinkedHashMap<Op, EAPPair<EAValType, EALType>> cases = visitCases(n);
        return tf.local.in(r, cases);
    }

    public EALOutType visitSelect(CommonTree n) {
        Role r = visitRole((CommonTree) n.getChild(0));
        LinkedHashMap<Op, EAPPair<EAValType, EALType>> cases = visitCases(n);
        return tf.local.out(r, cases);
    }

    public EALRecType visitRecursion(CommonTree n) {
        RecVar rv = visitRecVar((CommonTree) n.getChild(0));
        EALType body = visitSessionType((CommonTree) n.getChild(1));
        return tf.local.rec(rv, body);
    }

    public EALRecVarType visitRecVarType(CommonTree n) {  // cf. visitRecVar
        RecVar rv = visitRecVar((CommonTree) n.getChild(0));
        return tf.local.recvar(rv);
    }

    public RecVar visitRecVar(CommonTree n) {  // cf. visitRecVarType
        return new RecVar(n.getText());
    }

    // n is whole branch/select node
    @NotNull
    private LinkedHashMap<Op, EAPPair<EAValType, EALType>> visitCases(CommonTree n) {
        LinkedHashMap<Op, EAPPair<EAValType, EALType>> cases = new LinkedHashMap<>();
        for (int j = 1; j < n.getChildCount(); ) {
            Op op = visitOp((CommonTree) n.getChild(j));
            EAValType valType = visitA((CommonTree) n.getChild(j + 2));
            EALType body = visitSessionType((CommonTree) n.getChild(j + 5));
            cases.put(op, new EAPPair<>(valType, body));
            j = j + 6;
        }
        return cases;
    }

    /* names */

    public Role visitRole(CommonTree n) {
        return new Role(n.getText());
    }

    // FIXME only used by rec -- other occurrences of fnames come out as var...
    public EAPFuncName visitfname(CommonTree n) {
        return new EAPFuncName(n.getText());
    }

    public Op visitOp(CommonTree n) {
        return new Op(n.getText());
    }
}
