package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

public class EAPHandlers implements EAPVal {

    @NotNull public final Map<Pair<Op, EAPVar>, EAPExpr> Hbar;

    protected EAPHandlers(@NotNull LinkedHashMap<Pair<Op, EAPVar>, EAPExpr> Hbar) {
        this.Hbar = Collections.unmodifiableMap(Hbar.entrySet().stream().collect(
                Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new)));
    }

    @Override
    public EAPHandlers subs(@NotNull Map<EAPVar, EAPVal> m) {
        LinkedHashMap<Pair<Op, EAPVar>, EAPExpr> Hs = new LinkedHashMap<>();
        for (Map.Entry<Pair<Op, EAPVar>, EAPExpr> e : this.Hbar.entrySet()) {
            Map<EAPVar, EAPVal> m1 = new HashMap<>(m);
            Pair<Op, EAPVar> k = e.getKey();
            m1.remove(k.right);
            Hs.put(e.getKey(), e.getValue().subs(m1));
        }
        return EAPFactory.factory.handlers(Hs);
    }

    @Override
    public Set<EAPVar> getFreeVars() {
        Set<EAPVar> bound = this.Hbar.keySet().stream().map(x -> x.right)
                .collect(Collectors.toSet());
        Set<EAPVar> res = this.Hbar.values().stream()
                .flatMap(x -> x.getFreeVars().stream()).collect(Collectors.toSet());
        res.removeAll(bound);
        return res;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPHandlers eaVar = (EAPHandlers) o;
        return eaVar.canEquals(this) && this.Hbar.equals(eaVar.Hbar);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPHandlers;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.HANDLERS;
        hash = 31 * hash + this.Hbar.hashCode();
        return hash;
    }
}
