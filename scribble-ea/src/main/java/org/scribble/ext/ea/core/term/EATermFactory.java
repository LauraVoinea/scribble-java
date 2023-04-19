package org.scribble.ext.ea.core.term;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.term.expr.*;
import org.scribble.ext.ea.core.term.process.*;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVType;

import java.util.LinkedHashMap;

public class EATermFactory {

    public static final EATermFactory factory = new EATermFactory();

    protected EATermFactory() {

    }

    /* Values */

    public EAEHandler handler(@NotNull Op op, @NotNull EAEVar var, @NotNull EAVType varType,
                              @NotNull EAComp expr, @NotNull EALType pre,
                              @NotNull EAEVar svar, @NotNull EAVType svarType) {
        return new EAEHandler(op, var, varType, expr, pre, svar, svarType);
    }

    public EAEHandlers handlers(
            @NotNull Role role, @NotNull LinkedHashMap<Op, EAEHandler> Hs) {
        return new EAEHandlers(role, Hs);
    }

    public EAERec rec(@NotNull EAFuncName f, @NotNull EAEVar var,
                      @NotNull EAVType varType, @NotNull EAComp body,
                      @NotNull EALType S, @NotNull EALType T, @NotNull EAVType B) {
        return new EAERec(f, var, varType, body, S, T, B);
    }

    public EAEUnit unit() {
        return EAEUnit.UNIT;
    }

    public EAEVar var(@NotNull String id) {
        return new EAEVar(id);
    }

    public EAEIntVal intt(@NotNull int val) {
        return new EAEIntVal(val);
    }

    public EAEBoolVal bool(@NotNull boolean val) {
        return new EAEBoolVal(val);
    }

    public EAEBinOp binop(@NotNull EAEOp op, @NotNull EAExpr left, @NotNull EAExpr right) {
        return new EAEBinOp(op, left, right);
    }


    /* Computations */

    //public EAPLet let(@NotNull EAPVar var, @NotNull EAPExpr init, @NotNull EAPExpr body) {
    public EAPLet let(@NotNull EAEVar var, @NotNull EAVType varType,
                      @NotNull EAComp init, @NotNull EAComp body) {
        return new EAPLet(var, varType, init, body);
    }

    public EAPIf iff(@NotNull EAExpr cond, @NotNull EAComp then, @NotNull EAComp elsee) {
        return new EAPIf(cond, then, elsee);
    }

    public EAPApp app(@NotNull EAExpr left, @NotNull EAExpr right) {
        return new EAPApp(left, right);
    }

    public EAPReturn returnn(@NotNull EAExpr val) {
        return new EAPReturn(val);
    }

    public EAPSend send(@NotNull Role dst, @NotNull Op op, @NotNull EAExpr val) {
        return new EAPSend(dst, op, val);
    }

    public EAPSuspend suspend(@NotNull EAExpr val, @NotNull EAExpr sval) {
        return new EAPSuspend(val, sval);
    }
}
