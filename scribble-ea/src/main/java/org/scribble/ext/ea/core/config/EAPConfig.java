package org.scribble.ext.ea.core.config;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.process.EAPHandlers;
import org.scribble.ext.ea.core.process.EAPTerm;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

public class EAPConfig implements EAPRuntimeTerm {

    @NotNull public final EAPPid pid;
    @NotNull public final EAPThreadState T;
    @NotNull public final Map<Pair<EAPSid, Role>, EAPHandlers> sigma;  // !!! handlers specifically

    protected EAPConfig(@NotNull EAPPid pid,
                        @NotNull EAPThreadState T,
                        @NotNull LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> handlers) {
        this.pid = pid;
        this.T = T;
        this.sigma = Collections.unmodifiableMap(handlers.entrySet()
                .stream().collect(Collectors.toMap(
                        Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new)));
    }

    public boolean isActive() {
        return !this.T.isIdle();
    }

    /* Aux */

    @Override
    public String toString() {
        return "<" + this.pid + ", " + this.T + ", " + this.sigma + ">";
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPConfig them = (EAPConfig) o;
        return them.canEquals(this)
                && this.pid.equals(them.pid)
                && this.T.equals(them.T)
                && this.sigma.equals(them.sigma);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPConfig;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.CONFIG;
        hash = 31 * hash + this.pid.hashCode();
        hash = 31 * hash + this.T.hashCode();
        hash = 31 * hash + this.sigma.hashCode();
        return hash;

    }
}
