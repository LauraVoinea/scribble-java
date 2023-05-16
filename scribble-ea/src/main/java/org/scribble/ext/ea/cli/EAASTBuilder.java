package org.scribble.ext.ea.cli;


import org.antlr.runtime.tree.CommonTree;
import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.runtime.EARuntimeFactory;
import org.scribble.ext.ea.core.term.*;
import org.scribble.ext.ea.core.term.expr.*;
import org.scribble.ext.ea.core.term.comp.*;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.session.local.*;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.stream.Collectors;

// Move to parsing
public class EAASTBuilder {
    static EATermFactory pf = EATermFactory.factory;
    static EARuntimeFactory rf = EARuntimeFactory.factory;
    static EATypeFactory tf = EATypeFactory.factory;

    /*public EAPTerm visit(CommonTree n) {  // Entry
        return visitM(n);
    }*/

    /* M */

    public EAComp visitM(CommonTree n) {
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
            case "M_IF":
                return visitIf(n);
        }
        throw new RuntimeException("Unknown node kind: " + n.getText());
    }

    public EAMIf visitIf(CommonTree n) {
        EAExpr cond = visitV((CommonTree) n.getChild(0));
        EAComp then = visitM((CommonTree) n.getChild(1));
        EAComp elsee = visitM((CommonTree) n.getChild(2));
        return pf.iff(cond, then, elsee);
    }

    public EAMLet visitLet(CommonTree n) {
        EAEVar var = visitVar((CommonTree) n.getChild(0));
        EAVType varType = visitA((CommonTree) n.getChild(1));
        EAComp e1 = visitM((CommonTree) n.getChild(2));
        EAComp e2 = visitM((CommonTree) n.getChild(3));
        return pf.let(var, varType, e1, e2);
    }

    public EAMSend visitSend(CommonTree n) {
        Role dst = visitRole((CommonTree) n.getChild(0));
        Op op = visitOp((CommonTree) n.getChild(1));
        EAExpr V = visitV((CommonTree) n.getChild(2));
        return pf.send(dst, op, V);
    }

    public EAMSuspend visitSuspend(CommonTree n) {
        EAExpr V = visitV((CommonTree) n.getChild(0));
        EAExpr sV = visitV((CommonTree) n.getChild(1));
        return pf.suspend(V, sV);
    }

    public EAMReturn visitReturn(CommonTree n) {
        EAExpr V = visitV((CommonTree) n.getChild(0));
        return pf.returnn(V);
    }

    public EAMApp visitApp(CommonTree n) {
        EAExpr left = visitV((CommonTree) n.getChild(0));
        EAExpr right = visitV((CommonTree) n.getChild(1));
        return pf.app(left, right);
    }

    /* V */

    public EAExpr visitV(CommonTree n) {
        String txt = n.getText();
        switch (txt) {
            case "V_HANDLERS": {
                List<CommonTree> cs = ((List<Object>) n.getChildren()).stream()
                        .map(x -> (CommonTree) x).collect(Collectors.toList());
                Role r = visitRole(cs.get(0));
                LinkedHashMap<Op, EAHandler> hs = visitHandlers(cs.subList(1, cs.size()));
                return pf.handlers(r, hs);
            }
            case "V_INT":
                return visitInt(n);
            case "V_VAR":
                return visitVar(n);
            case "V_UNIT":
                return pf.unit();
            case "V_TRUE":
                return pf.bool(true);
            case "V_FALSE":
                return pf.bool(false);
            case "V_REC": {
                EAEFuncName f = visitfname((CommonTree) n.getChild(0));
                EAEVar var = visitVar((CommonTree) n.getChild(1));
                EAVType varType = visitA((CommonTree) n.getChild(2));
                EALType S = visitSessionType((CommonTree) n.getChild(3));
                EALType T = visitSessionType((CommonTree) n.getChild(4));
                EAVType B = visitA((CommonTree) n.getChild(5));
                EAComp body = visitM((CommonTree) n.getChild(6));
                return pf.rec(f, var, varType, body, S, T, B);
            }
            case "V_PLUS":
                return visitPlus(n);
            case "V_COMP":
                return visitComp(n);
            default:
                throw new RuntimeException("Unknown V: " + n);
        }
    }

    // FIXME other comp ops
    public EAExpr visitComp(CommonTree n) {
        List<Object> cs = n.getChildren();
        EAExpr curr = visitV((CommonTree) cs.get(0));
        for (int i = 1; i < cs.size(); i++) {
            curr = EATermFactory.factory.binop(EAOp.LT, curr, visitV((CommonTree) cs.get(i)));
        }
        return curr;
    }

    public EAExpr visitPlus(CommonTree n) {
        List<Object> cs = n.getChildren();
        EAExpr curr = visitV((CommonTree) cs.get(0));
        for (int i = 1; i < cs.size(); i++) {
            curr = EATermFactory.factory.binop(EAOp.PLUS, curr, visitV((CommonTree) cs.get(i)));
        }
        return curr;
    }

    public EAEVar visitVar(CommonTree n) {
        //return pf.var(n.getText());
        return pf.var(n.getChild(0).getText());
    }

    /*public EAPBoolVal visitBool(CommonTree n) {
        String txt = n.getText();
        return pf.bool(txt.equals("V_TRUE"));
    }*/

    public EAEIntVal visitInt(CommonTree n) {
        String txt = n.getChild(0).getText();
        if (txt.equals("UNIT_KW")) {  // FIXME
            return pf.intt(1);
        }
        return pf.intt(Integer.parseInt(txt));
    }

    public LinkedHashMap<Op, EAHandler> visitHandlers(List<CommonTree> n) {
        return n.stream().map(this::visitHandler).collect(Collectors.toMap(
                x -> x.op,
                x -> x,
                (x, y) -> null,
                LinkedHashMap::new
        ));
    }

    public EAHandler visitHandler(CommonTree n) {
        Op op = visitOp((CommonTree) n.getChild(0));
        EAEVar var = visitVar((CommonTree) n.getChild(1));
        EAVType varType = visitA((CommonTree) n.getChild(2));
        EALType stype = visitSessionType((CommonTree) n.getChild(3));
        EAComp expr = visitM((CommonTree) n.getChild(4));
        EAEVar svar = visitVar((CommonTree) n.getChild(5));
        EAVType svarType = visitA((CommonTree) n.getChild(6));
        return pf.handler(op, var, varType, expr, stype, svar, svarType);
    }

    /* A */

    public EAVType visitA(CommonTree n) {
        String txt = n.getText();
        switch (txt) {
            case "A_HANDLER": {
                EAVType A = visitA((CommonTree) n.getChild(0));
                EALInType inS = (EALInType) visitSessionType((CommonTree) n.getChild(1));
                return tf.val.handlers(inS, A);
            }
            case "A_UNIT": {
                return tf.val.unit();
            }
            case "A_FUN": {
                EAVType A = visitA((CommonTree) n.getChild(0));
                EALType S = visitSessionType((CommonTree) n.getChild(1));
                EALType T = visitSessionType((CommonTree) n.getChild(2));
                EAVType B = visitA((CommonTree) n.getChild(3));
                return tf.val.func(A, S, T, B);
            }
            case "A_INT":
                return tf.val.intt();
            case "A_BOOL":
                return tf.val.bool();
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
        LinkedHashMap<Op, Pair<EAVType, EALType>> cases = visitCases(n);
        return tf.local.in(r, cases);
    }

    public EALOutType visitSelect(CommonTree n) {
        Role r = visitRole((CommonTree) n.getChild(0));
        LinkedHashMap<Op, Pair<EAVType, EALType>> cases = visitCases(n);
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
    private LinkedHashMap<Op, Pair<EAVType, EALType>> visitCases(CommonTree n) {
        LinkedHashMap<Op, Pair<EAVType, EALType>> cases = new LinkedHashMap<>();
        for (int j = 1; j < n.getChildCount(); ) {
            Op op = visitOp((CommonTree) n.getChild(j));
            EAVType valType = visitA((CommonTree) n.getChild(j + 2));
            EALType body = visitSessionType((CommonTree) n.getChild(j + 5));
            cases.put(op, new Pair<>(valType, body));
            j = j + 6;
        }
        return cases;
    }

    /* names */

    public Role visitRole(CommonTree n) {
        return new Role(n.getText());
    }

    // FIXME only used by rec -- other occurrences of fnames come out as var...
    public EAEFuncName visitfname(CommonTree n) {
        return new EAEFuncName(n.getText());
    }

    public Op visitOp(CommonTree n) {
        return new Op(n.getText());
    }
}
