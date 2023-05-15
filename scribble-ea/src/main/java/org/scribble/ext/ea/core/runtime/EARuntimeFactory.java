package org.scribble.ext.ea.core.runtime;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.local.LTypeFactory;
import org.scribble.ext.ea.core.runtime.config.EACActor;
import org.scribble.ext.ea.core.term.expr.EAEHandlers;
import org.scribble.ext.ea.core.term.expr.EAExpr;
import org.scribble.ext.ea.core.term.comp.EAComp;
import org.scribble.ext.ea.core.type.session.local.Delta;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;
import java.util.Map;

public class EARuntimeFactory {

    public static final EARuntimeFactory factory = new EARuntimeFactory();

    protected EARuntimeFactory() {
    }

    public EAPid pid(@NotNull String id) {
        return new EAPid(id);
    }

    public EASid sid(String id) {
        return new EASid(id);
    }

    public EACActor config(EAPid pid, EAThread T,
                           LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigma,
                           //LinkedHashMap<Pair<EAPSid, Role>, Integer> state) {
                           EAExpr state) {
        return new EACActor(pid, T, sigma, state);
    }

    public EATIdle idle() {
        return EATIdle.IDLE;
    }

    public EATActive activeThread(
            @NotNull EAComp expr, @NotNull EASid sid, @NotNull Role role) {
        return new EATActive(expr, sid, role);
    }

    public EASystem system(@NotNull LTypeFactory lf,
                           @NotNull Delta annots,
                           @NotNull LinkedHashMap<EAPid, EACActor> cs) {
        return new EASystem(lf, annots, cs);
    }
}
