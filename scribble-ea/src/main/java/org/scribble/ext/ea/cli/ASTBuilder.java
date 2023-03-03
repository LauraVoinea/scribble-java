package org.scribble.ext.ea.cli;


import org.antlr.runtime.tree.CommonTree;
import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.config.EAPRuntimeFactory;
import org.scribble.ext.ea.core.process.*;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.session.local.EALInType;
import org.scribble.ext.ea.core.type.session.local.EALOutType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.util.EAPPair;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.stream.Collectors;

class ASTBuilder {
    static EAPFactory pf = EAPFactory.factory;
    static EAPRuntimeFactory rf = EAPRuntimeFactory.factory;
    static EATypeFactory tf = EATypeFactory.factory;

    /*public EAPTerm visit(CommonTree n) {  // Entry
        return visitM(n);
    }*/

    public EAPExpr visitM(CommonTree n) {
        switch (n.getText()) {
            case "M_LET": return visitLet(n);
            case "M_SEND": return visitSend(n);
            case "M_SUSPEND": return visitSuspend(n);
            case "M_RETURN": return visitReturn(n);
        }
        throw new RuntimeException("Unknown node kind: " + n.getText());
    }

    public EAPLet visitLet(CommonTree n) {
        EAPVar var = visitVar((CommonTree) n.getChild(0));
        EAValType varType = visitValType((CommonTree) n.getChild(1));
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
            case "V_UNIT": return pf.unit();
            default: return visitVar(n);
        }
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
        EAValType varType = visitValType((CommonTree) n.getChild(2));
        EALType stype = visitSessionType((CommonTree) n.getChild(3));
        EAPExpr expr = visitM((CommonTree) n.getChild(4));
        return pf.handler(op, var, varType, expr, stype);
    }

    public EAValType visitValType(CommonTree n) {
        String txt = n.getText();
        switch (txt) {
            case "1": {
                return tf.val.unit();
            }
            default: throw new RuntimeException("Unknown val type: " + n);
        }
    }

    public EAPVar visitVar(CommonTree n) {
        return pf.var(n.getText());
    }

    public Role visitRole(CommonTree n) {
        return new Role(n.getText());
    }

    public Op visitOp(CommonTree n) {
        return new Op(n.getText());
    }

    public EALType visitSessionType(CommonTree n) {
        String txt = n.getText();
        switch (txt) {
            case "S_END": return tf.local.end();
            case "S_BRANCH": return visitBranch(n);
            case "S_SELECT": return visitSelect(n);
            default: throw new RuntimeException("Unknown session type: " + n);
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

    // n is whole branch/select node
    @NotNull
    private LinkedHashMap<Op, EAPPair<EAValType, EALType>> visitCases(CommonTree n) {
        LinkedHashMap<Op, EAPPair<EAValType, EALType>> cases = new LinkedHashMap<>();
        for (int j = 1; j < n.getChildCount(); ) {
            Op op = visitOp((CommonTree) n.getChild(j));
            EAValType valType = visitValType((CommonTree) n.getChild(j+2));
            EALType body = visitSessionType((CommonTree) n.getChild(j+5));
            cases.put(op, new EAPPair<>(valType, body));
            j = j+6;
        }
        return cases;
    }
}
