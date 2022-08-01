package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.EALEndType;
import org.scribble.ext.ea.core.type.session.local.EALInType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.session.local.EALTypeFactory;
import org.scribble.ext.ea.core.type.value.EAUnitType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.core.type.value.EAValTypeFactory;
import org.scribble.ext.ea.util.EAPPair;
import org.scribble.ext.ea.util.EATriple;
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
    @NotNull public final Map<Op, EATriple<EAPVar, EAValType, EAPExpr>> Hs;  // !!! added type annot

    protected EAPHandlers(
            @NotNull Role role,
            @NotNull LinkedHashMap<Op, EATriple<EAPVar, EAValType, EAPExpr>> Hbar) {
        this.role = role;
        this.Hs = Collections.unmodifiableMap(Hbar.entrySet().stream().collect(
                Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new)));
    }

    @Override
    public EAValType type(Gamma gamma) {
        LinkedHashMap<Op, Pair<EAValType, EALType>> cases = new LinkedHashMap<>();
        for (Map.Entry<Op, EATriple<EAPVar, EAValType, EAPExpr>> e : Hs.entrySet()) {
           Op k = e.getKey();
           EATriple<EAPVar, EAValType, EAPExpr> v = e.getValue();
           LinkedHashMap<EAName, EAValType> tmp = new LinkedHashMap<>(gamma.map);
           tmp.put(v.left, v.mid);
           Gamma gamma1 = new Gamma(tmp);

           EALType inferred = v.right.infer(gamma1);

           Pair<EAValType, EALType> res = v.right.type(gamma1, inferred);
           if (!(res.left.equals(EAUnitType.UNIT)) || !(res.right.equals(EALEndType.END))) {
               throw new RuntimeException("Type error: " + gamma1 + " | "
                       + inferred + " |>" + v.right + ":" + res.left + " <|" + res.right);
           }
           cases.put(k, new EAPPair<EAValType, EALType>(v.mid, inferred));
       }
        EALInType in = EALTypeFactory.factory.in(this.role, cases);
        return EAValTypeFactory.factory.handlers(in);
    }

    /* Aux */

    @Override
    public EAPHandlers subs(@NotNull Map<EAPVar, EAPVal> m) {
        LinkedHashMap<Op, EATriple<EAPVar, EAValType, EAPExpr>> Hs = new LinkedHashMap<>();
        for (Map.Entry<Op, EATriple<EAPVar, EAValType, EAPExpr>> e : this.Hs.entrySet()) {
            Map<EAPVar, EAPVal> m1 = new HashMap<>(m);
            Op k = e.getKey();
            EATriple<EAPVar, EAValType, EAPExpr> v = e.getValue();
            m1.remove(v.left);
            Hs.put(k, new EATriple<EAPVar, EAValType, EAPExpr>(v.left, v.mid, v.right.subs(m1)));
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

    private static String handlerToString(Op k, EATriple<EAPVar, EAValType, EAPExpr> e) {
        return k + "(" + e.left + ":" + e.mid + ") |-> " + e.right;
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
