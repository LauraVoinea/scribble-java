package org.scribble.ext.ea.core.config;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.local.LTypeFactory;
import org.scribble.ext.ea.core.process.*;
import org.scribble.ext.ea.core.type.session.local.Delta;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;
import java.util.Map;

public class EAPRuntimeFactory {

    public static final EAPRuntimeFactory factory = new EAPRuntimeFactory();

    protected EAPRuntimeFactory() {
    }

    public EAPPid pid(@NotNull String id) {
        return new EAPPid(id);
    }

    public EAPSid sid(String id) {
        return new EAPSid(id);
    }

    public EAPConfig config(EAPPid pid, EAPThreadState T,
                            LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigma,
                            //LinkedHashMap<Pair<EAPSid, Role>, Integer> state) {
                            EAPVal state) {
        return new EAPConfig(pid, T, sigma, state);
    }

    public EAPIdle idle() {
        return EAPIdle.IDLE;
    }

    public EAPActiveThread activeThread(
            @NotNull EAPExpr expr, @NotNull EAPSid sid, @NotNull Role role) {
        return new EAPActiveThread(expr, sid, role);
    }

    public EAPSystem system(@NotNull LTypeFactory lf,
                            @NotNull Delta annots,
                            @NotNull LinkedHashMap<EAPPid, EAPConfig> cs) {
        return new EAPSystem(lf, annots, cs);
    }
}
