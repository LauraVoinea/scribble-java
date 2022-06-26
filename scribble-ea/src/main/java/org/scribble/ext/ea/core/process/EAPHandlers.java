package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

//
// fundamental essence of MPST handlers?  here: actors vs. processes?
//      handler per session per actor, cf. ED-process handler for all sessions
//      => `become` -- how about progress? -- what is aim of `become`?
// alternative: multi-session actor handlers?
//

//HERE
//- if-else
//- configs and config exec
//- newAP/spawn/register

public class EAPHandlers implements EAPVal {

    @NotNull public final Role role;
    //@NotNull public final Map<Pair<Op, EAPVar>, EAPExpr> Hs;  // !!! var is part of value, not key
    @NotNull public final Map<Op, Pair<EAPVar, EAPExpr>> Hs;

    protected EAPHandlers(
            @NotNull Role role,
            @NotNull LinkedHashMap<Op, Pair<EAPVar, EAPExpr>> Hbar) {
        this.role = role;
        this.Hs = Collections.unmodifiableMap(Hbar.entrySet().stream().collect(
                Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new)));
    }

    /* Aux */

    @Override
    public EAPHandlers subs(@NotNull Map<EAPVar, EAPVal> m) {
        LinkedHashMap<Op, Pair<EAPVar, EAPExpr>> Hs = new LinkedHashMap<>();
        for (Map.Entry<Op, Pair<EAPVar, EAPExpr>> e : this.Hs.entrySet()) {
            Map<EAPVar, EAPVal> m1 = new HashMap<>(m);
            Op k = e.getKey();
            Pair<EAPVar, EAPExpr> v = e.getValue();
            m1.remove(v.left);
            Hs.put(k, new Pair<EAPVar, EAPExpr>(v.left, v.right.subs(m1)));
        }
        return EAPFactory.factory.handlers(this.role, Hs);
    }

    @Override
    public Set<EAPVar> getFreeVars() {
        Set<EAPVar> res = this.Hs.values().stream().flatMap(x -> {
            Set<EAPVar> fvs = x.right.getFreeVars();
            fvs.remove(x.left);
            return fvs.stream();
        }).collect(Collectors.toSet());
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

    private static String handlerToString(Op k, Pair<EAPVar, EAPExpr> e) {
        return k + "(" + e.left + ") |-> " + e.right;
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
