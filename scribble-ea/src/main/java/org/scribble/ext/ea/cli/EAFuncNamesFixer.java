package org.scribble.ext.ea.cli;

import org.scribble.core.type.name.Op;
import org.scribble.ext.ea.core.config.EAPPid;
import org.scribble.ext.ea.core.config.EAPSid;
import org.scribble.ext.ea.core.process.*;

import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class EAFuncNamesFixer {

    protected static final EAPFactory f = EAPFactory.factory;

    public EAPExpr parse(EAPExpr M) {
        return visit(M, new HashSet<>());
    }

    public EAPVal parse(EAPVal V) {
        return visit(V, new HashSet<>());
    }

    protected EAPExpr visit(EAPExpr M, Set<EAPFuncName> env) {
        if (M instanceof EAPLet) {
            EAPLet cast = (EAPLet) M;
            EAPVar var = (EAPVar) visit(cast.var, env);
            EAPExpr init = visit(cast.init, env);
            EAPExpr body = visit(cast.body, env);
            return f.let(var, cast.varType, init, body);
        } else if (M instanceof EAPIf) {
            EAPIf cast = (EAPIf) M;
            EAPVal cond = visit(cast.cond, env);
            EAPExpr then = visit(cast.then, env);
            EAPExpr elsee = visit(cast.elsee, env);
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

    protected EAPVal visit(EAPVal V, Set<EAPFuncName> env) {
        if (V instanceof EAPHandlers) {
            EAPHandlers cast = (EAPHandlers) V;
            LinkedHashMap<Op, EAPHandler> Hs =
                    cast.Hs.entrySet().stream().collect(Collectors.toMap(
                            Map.Entry::getKey,
                            x -> visit(x.getValue(), env),
                            (x, y) -> null,
                            LinkedHashMap::new
                    ));
            return f.handlers(cast.role, Hs);
        } else if (V instanceof EAPVar) {
            EAPFuncName tmp = new EAPFuncName(((EAPVar) V).id);
            return env.contains(tmp) ? tmp : V;
        } else if (V instanceof EAPRec) {
            EAPRec cast = (EAPRec) V;
            Set<EAPFuncName> tmp = new HashSet<>(env);
            tmp.add(cast.f);
            EAPVar var = (EAPVar) visit(cast.var, tmp);
            EAPExpr body = visit(cast.body, tmp);
            return f.rec(cast.f, var, cast.varType, body, cast.S, cast.T, cast.B);
        } else if (V instanceof EAPFuncName || V instanceof EAPUnit
                || V instanceof EAPPid || V instanceof EAPSid
                || V instanceof EAPIntVal || V instanceof EAPBinOp || V instanceof EAPBoolVal) {
            return V;
        } else {
            throw new RuntimeException("TODO: " + V);
        }
    }

    public EAPHandler visit(EAPHandler H, Set<EAPFuncName> env) {
        EAPVar var = (EAPVar) visit(H.var, env);
        EAPVar svar = (EAPVar) visit(H.svar, env);
        EAPExpr expr = visit(H.expr, env);
        return f.handler(H.op, var, H.varType, expr, H.pre, svar, H.svarType);
    }
}
