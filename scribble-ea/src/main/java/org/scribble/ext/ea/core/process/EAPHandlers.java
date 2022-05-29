package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

public class EAPHandlers implements EAPVal {

    @NotNull public final Role role;
    @NotNull public final Map<Pair<Op, EAPVar>, EAPExpr> Hs;

    protected EAPHandlers(
            @NotNull Role role,
            @NotNull LinkedHashMap<Pair<Op, EAPVar>, EAPExpr> Hbar) {
        this.role = role;
        this.Hs = Collections.unmodifiableMap(Hbar.entrySet().stream().collect(
                Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new)));
    }

    /* Aux */

    @Override
    public EAPHandlers subs(@NotNull Map<EAPVar, EAPVal> m) {
        LinkedHashMap<Pair<Op, EAPVar>, EAPExpr> Hs = new LinkedHashMap<>();
        for (Map.Entry<Pair<Op, EAPVar>, EAPExpr> e : this.Hs.entrySet()) {
            Map<EAPVar, EAPVal> m1 = new HashMap<>(m);
            Pair<Op, EAPVar> k = e.getKey();
            m1.remove(k.right);
            Hs.put(e.getKey(), e.getValue().subs(m1));
        }
        return EAPFactory.factory.handlers(this.role, Hs);
    }

    @Override
    public Set<EAPVar> getFreeVars() {
        Set<EAPVar> bound = this.Hs.keySet().stream().map(x -> x.right)
                .collect(Collectors.toSet());
        Set<EAPVar> res = this.Hs.values().stream()
                .flatMap(x -> x.getFreeVars().stream()).collect(Collectors.toSet());
        res.removeAll(bound);
        return res;
    }

    @Override
    public String toString() {
        return "handler " + this.role + " { "
                + this.Hs.entrySet().stream()
                .map(x -> handlerToString(x.getKey(), x.getValue()))
                .collect(Collectors.joining(", "))
                + " }";
    }

    private static String handlerToString(Pair<Op, EAPVar> k, EAPExpr e) {
        return k.left + "(" + k.right + ") |-> " + e;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPHandlers them = (EAPHandlers) o;
        return them.canEquals(this)
                && this.role.equals(them.role)
                && this.Hs.equals(them.Hs);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPHandlers;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.HANDLERS;
        hash = 31 * hash + this.role.hashCode();
        hash = 31 * hash + this.Hs.hashCode();
        return hash;
    }
}
