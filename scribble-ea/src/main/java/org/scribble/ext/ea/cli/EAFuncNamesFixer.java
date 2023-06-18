package org.scribble.ext.ea.cli;

import org.scribble.core.type.name.Op;
import org.scribble.ext.ea.core.runtime.EAPid;
import org.scribble.ext.ea.core.runtime.EASid;
import org.scribble.ext.ea.core.term.EATermFactory;
import org.scribble.ext.ea.core.term.comp.*;
import org.scribble.ext.ea.core.term.expr.*;

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

    protected EAComp visit(EAComp M, Set<EAEFuncName> env) {
        if (M instanceof EAMLet) {
            EAMLet cast = (EAMLet) M;
            EAEVar var = (EAEVar) visit(cast.var, env);
            EAComp init = visit(cast.init, env);
            EAComp body = visit(cast.body, env);
            return f.let(var, cast.varType, init, body);
        } else if (M instanceof EAMIf) {
            EAMIf cast = (EAMIf) M;
            EAExpr cond = visit(cast.cond, env);
            EAComp then = visit(cast.then, env);
            EAComp elsee = visit(cast.elsee, env);
            return f.iff(cond, then, elsee);
        } else if (M instanceof EAMSuspend) {
            EAMSuspend cast = (EAMSuspend) M;
            return f.suspend(visit(cast.val, env), visit(cast.sval, env));
        } else if (M instanceof EAMApp) {
            EAMApp cast = (EAMApp) M;
            return f.app(visit(cast.left, env), visit(cast.right, env));
        } else if (M instanceof EAMReturn) {
            return f.returnn(visit(((EAMReturn) M).val, env));
        } else if (M instanceof EAMSend) {
            EAMSend cast = (EAMSend) M;
            return f.send(cast.dst, cast.op, visit(cast.val, env));
        } else if (M instanceof EAMSpawn) {
            EAMSpawn cast = (EAMSpawn) M;
            return f.spawn(visit(cast.M, env));
        } else if (M instanceof EAMRegister) {
            EAMRegister cast = (EAMRegister) M;
            return f.register(visit(cast.V, env), cast.role, visit(cast.M, env));
        } else if (M instanceof EAMBinOp) {
            EAMBinOp cast = (EAMBinOp) M;
            return f.binop(cast.op, visit(cast.left, env), visit(cast.right, env));
        } else {
            throw new RuntimeException("TODO: " + M);
        }
    }

    protected EAExpr visit(EAExpr V, Set<EAEFuncName> env) {
        if (V instanceof EAEHandlers) {
            EAEHandlers cast = (EAEHandlers) V;
            LinkedHashMap<Op, EAHandler> Hs =
                    cast.Hs.entrySet().stream().collect(Collectors.toMap(
                            Map.Entry::getKey,
                            x -> visit(x.getValue(), env),
                            (x, y) -> null,
                            LinkedHashMap::new
                    ));
            return f.handlers(cast.role, Hs);
        } else if (V instanceof EAEVar) {
            EAEFuncName tmp = new EAEFuncName(((EAEVar) V).id);
            return env.contains(tmp) ? tmp : V;
        } else if (V instanceof EAERec) {
            EAERec cast = (EAERec) V;
            Set<EAEFuncName> tmp = new HashSet<>(env);
            tmp.add(cast.f);
            EAEVar var = (EAEVar) visit(cast.var, tmp);
            EAComp body = visit(cast.body, tmp);
            return f.rec(cast.f, var, cast.varType, body, cast.S, cast.T, cast.B);
        } else if (V instanceof EAEFuncName || V instanceof EAEUnit
                || V instanceof EAPid || V instanceof EASid
                || V instanceof EAEIntVal || V instanceof EAEBoolVal
                || V instanceof EAEAPName) {
            return V;
        } else {
            throw new RuntimeException("TODO: " + V.getClass() + " ,, " + V);
        }
    }

    public EAHandler visit(EAHandler H, Set<EAEFuncName> env) {
        EAEVar var = (EAEVar) visit(H.var, env);
        EAEVar svar = (EAEVar) visit(H.svar, env);
        EAComp expr = visit(H.expr, env);
        return f.handler(H.op, var, H.varType, expr, H.pre, svar, H.svarType);
    }
}
