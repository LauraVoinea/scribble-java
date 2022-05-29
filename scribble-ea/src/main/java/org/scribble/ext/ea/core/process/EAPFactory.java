package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;

public class EAPFactory {

    public static final EAPFactory factory = new EAPFactory();

    protected EAPFactory() {

    }

    public EAPHandlers handlers(
            @NotNull Role role,
            @NotNull LinkedHashMap<Pair<Op, EAPVar>, EAPExpr> Hs) {
        return new EAPHandlers(role, Hs);
    }

    //public EAPLet let(@NotNull EAPVar var, @NotNull EAPExpr init, @NotNull EAPExpr body) {
        public EAPLet let(@NotNull EAPVar var, @NotNull EAPVal init, @NotNull EAPExpr body) {
        return new EAPLet(var, init, body);
    }

    public EAPPid pid(@NotNull String id) {
        return new EAPPid(id);
    }

    public EAPReturn returnn(@NotNull EAPVal val) {
        return new EAPReturn(val);
    }

    public EAPSend send(@NotNull EAPVal val) {
        return new EAPSend(val);
    }

    public EAPSuspend suspend(@NotNull EAPVal val) {
        return new EAPSuspend(val);
    }

    public EAPUnit unit() {
        return EAPUnit.UNIT;
    }

    public EAPVar var(@NotNull String id) {
        return new EAPVar(id);
    }
}
