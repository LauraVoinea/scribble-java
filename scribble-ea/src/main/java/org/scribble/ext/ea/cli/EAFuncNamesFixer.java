package org.scribble.ext.ea.cli;

import org.scribble.core.type.name.Op;
import org.scribble.ext.ea.core.config.EAPPid;
import org.scribble.ext.ea.core.config.EAPSid;
import org.scribble.ext.ea.core.term.*;
import org.scribble.ext.ea.core.term.expr.*;
import org.scribble.ext.ea.core.term.process.*;

import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class EAFuncNamesFixer {

    protected static final EATermFactory f = EATermFactory.factory;

    public EAComp parse(EAComp M) {
        return visit(M, new HashSet<>());
    }

    public EAExpr parse(EAExpr V) {
        return visit(V, new HashSet<>());
    }

    protected EAComp visit(EAComp M, Set<EAFuncName> env) {
        if (M instanceof EAPLet) {
            EAPLet cast = (EAPLet) M;
            EAEVar var = (EAEVar) visit(cast.var, env);
            EAComp init = visit(cast.init, env);
            EAComp body = visit(cast.body, env);
            return f.let(var, cast.varType, init, body);
        } else if (M instanceof EAPIf) {
            EAPIf cast = (EAPIf) M;
            EAExpr cond = visit(cast.cond, env);
            EAComp then = visit(cast.then, env);
            EAComp elsee = visit(cast.elsee, env);
            return f.iff(cond, then, elsee);
        } else if (M instanceof EAPSuspend) {
            EAPSuspend cast = (EAPSuspend) M;
            return f.suspend(visit(cast.val, env), visit(cast.sval, env));
        } else if (M instanceof EAPApp) {
            EAPApp cast = (EAPApp) M;
            return f.app(visit(cast.left, env), visit(cast.right, env));
        } else if (M instanceof EAPReturn) {
            return f.returnn(visit(((EAPReturn) M).val, env));
        } else if (M instanceof EAPSend) {
            EAPSend cast = (EAPSend) M;
            return f.send(cast.dst, cast.op, visit(cast.val, env));
        } else {
            throw new RuntimeException("TODO: " + M);
        }
    }

    protected EAExpr visit(EAExpr V, Set<EAFuncName> env) {
        if (V instanceof EAEHandlers) {
            EAEHandlers cast = (EAEHandlers) V;
            LinkedHashMap<Op, EAEHandler> Hs =
                    cast.Hs.entrySet().stream().collect(Collectors.toMap(
                            Map.Entry::getKey,
                            x -> visit(x.getValue(), env),
                            (x, y) -> null,
                            LinkedHashMap::new
                    ));
            return f.handlers(cast.role, Hs);
        } else if (V instanceof EAEVar) {
            EAFuncName tmp = new EAFuncName(((EAEVar) V).id);
            return env.contains(tmp) ? tmp : V;
        } else if (V instanceof EAERec) {
            EAERec cast = (EAERec) V;
            Set<EAFuncName> tmp = new HashSet<>(env);
            tmp.add(cast.f);
            EAEVar var = (EAEVar) visit(cast.var, tmp);
            EAComp body = visit(cast.body, tmp);
            return f.rec(cast.f, var, cast.varType, body, cast.S, cast.T, cast.B);
        } else if (V instanceof EAFuncName || V instanceof EAEUnit
                || V instanceof EAPPid || V instanceof EAPSid
                || V instanceof EAEIntVal || V instanceof EAEBinOp || V instanceof EAEBoolVal) {
            return V;
        } else {
            throw new RuntimeException("TODO: " + V);
        }
    }

    public EAEHandler visit(EAEHandler H, Set<EAFuncName> env) {
        EAEVar var = (EAEVar) visit(H.var, env);
        EAEVar svar = (EAEVar) visit(H.svar, env);
        EAComp expr = visit(H.expr, env);
        return f.handler(H.op, var, H.varType, expr, H.pre, svar, H.svarType);
    }
}
