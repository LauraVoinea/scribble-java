package org.scribble.ext.ea.core.config;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.process.*;
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
                            LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigma) {
        return new EAPConfig(pid, T, sigma);
    }

    public EAPIdle idle() {
        return EAPIdle.IDLE;
    }

    public EAPActiveThread activeThread(
            @NotNull EAPExpr expr, @NotNull EAPSid sid, @NotNull Role role) {
        return new EAPActiveThread(expr, sid, role);
    }

    public EAPSystem system(LinkedHashMap<EAPPid, EAPConfig> cs) {
        return new EAPSystem(cs);
    }
}
