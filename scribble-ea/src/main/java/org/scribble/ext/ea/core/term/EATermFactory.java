package org.scribble.ext.ea.core.term;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.term.expr.*;
import org.scribble.ext.ea.core.term.comp.*;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVType;

import java.util.LinkedHashMap;

public class EATermFactory {

    public static final EATermFactory factory = new EATermFactory();

    protected EATermFactory() {

    }

    /* Values */

    public EAHandler handler(@NotNull Op op, @NotNull EAEVar var, @NotNull EAVType varType,
                             @NotNull EAComp expr, @NotNull EALType pre,
                             @NotNull EAEVar svar, @NotNull EAVType svarType) {
        return new EAHandler(op, var, varType, expr, pre, svar, svarType);
    }

    public EAEHandlers handlers(
            @NotNull Role role, @NotNull LinkedHashMap<Op, EAHandler> Hs) {
        return new EAEHandlers(role, Hs);
    }

    public EAERec rec(@NotNull EAEFuncName f, @NotNull EAEVar var,
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

    public EAEBinOp binop(@NotNull EAOp op, @NotNull EAExpr left, @NotNull EAExpr right) {
        return new EAEBinOp(op, left, right);
    }


    /* Computations */

    //public EAPLet let(@NotNull EAPVar var, @NotNull EAPExpr init, @NotNull EAPExpr body) {
    public EAMLet let(@NotNull EAEVar var, @NotNull EAVType varType,
                      @NotNull EAComp init, @NotNull EAComp body) {
        return new EAMLet(var, varType, init, body);
    }

    public EAMIf iff(@NotNull EAExpr cond, @NotNull EAComp then, @NotNull EAComp elsee) {
        return new EAMIf(cond, then, elsee);
    }

    public EAMApp app(@NotNull EAExpr left, @NotNull EAExpr right) {
        return new EAMApp(left, right);
    }

    public EAMReturn returnn(@NotNull EAExpr val) {
        return new EAMReturn(val);
    }

    public EAMSend send(@NotNull Role dst, @NotNull Op op, @NotNull EAExpr val) {
        return new EAMSend(dst, op, val);
    }

    public EAMSuspend suspend(@NotNull EAExpr val, @NotNull EAExpr sval) {
        return new EAMSuspend(val, sval);
    }
}
