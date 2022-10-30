package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Id;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.util.EATriple;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;

public class EAPFactory {

    public static final EAPFactory factory = new EAPFactory();

    protected EAPFactory() {

    }

    /* Values */

    public EAPHandler handler(@NotNull Op op, @NotNull EAPVar var, @NotNull EAValType varType,
                              @NotNull EAPExpr expr, @NotNull EALType pre) {
        return new EAPHandler(op, var, varType, expr, pre);
    }

    public EAPHandlers handlers(
            @NotNull Role role, @NotNull LinkedHashMap<Op, EAPHandler> Hs) {
        return new EAPHandlers(role, Hs);
    }

    public EAPRec rec(@NotNull EAPFuncName f, @NotNull EAPVar var,
            @NotNull EAValType varType, @NotNull EAPExpr body,
            @NotNull EALType S, @NotNull EALType T, @NotNull EAValType B) {
        return new EAPRec(f, var, varType, body, S, T, B);
    }

    public EAPUnit unit() {
        return EAPUnit.UNIT;
    }

    public EAPVar var(@NotNull String id) {
        return new EAPVar(id);
    }


    /* Computations */

    //public EAPLet let(@NotNull EAPVar var, @NotNull EAPExpr init, @NotNull EAPExpr body) {
    public EAPLet let(@NotNull EAPVar var, @NotNull EAValType varType,
                      @NotNull EAPExpr init, @NotNull EAPExpr body) {
        return new EAPLet(var, varType, init, body);
    }

    public EAPApp app(@NotNull EAPVal left, @NotNull EAPVal right) {
        return new EAPApp(left, right);
    }

    public EAPReturn returnn(@NotNull EAPVal val) {
        return new EAPReturn(val);
    }

    public EAPSend send(@NotNull Role dst, @NotNull Op op, @NotNull EAPVal val) {
        return new EAPSend(dst, op, val);
    }

    public EAPSuspend suspend(@NotNull EAPVal val) {
        return new EAPSuspend(val);
    }
}
