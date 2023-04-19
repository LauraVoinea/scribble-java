package org.scribble.ext.ea.core.term.expr;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.term.EAPFactory;
import org.scribble.ext.ea.core.term.EAPFuncName;
import org.scribble.ext.ea.core.term.EAPTerm;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.EALInType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.session.local.EALTypeFactory;
import org.scribble.ext.ea.core.type.value.EAHandlersType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.core.type.value.EAValTypeFactory;
import org.scribble.ext.ea.util.EAPPair;

import java.util.*;
import java.util.stream.Collectors;

//
// fundamental essence of MPST handlers?  here: actors vs. processes?
//      handler per session per actor, cf. ED-process handler for all sessions
//      => `become` -- how about progress? -- what is aim of `become`?
// alternative: multi-session actor handlers?
//


public class EAPHandlers implements EAPExpr {

    @NotNull
    public final Role role;
    //@NotNull public final Map<Pair<Op, EAPVar>, EAPExpr> Hs;  // !!! var is part of value, not key
    @NotNull
    public final Map<Op, EAPHandler> Hs;  // Invariant: Op equals EAPHandler.op

    public EAPHandlers(
            @NotNull Role role, @NotNull LinkedHashMap<Op, EAPHandler> Hbar) {
        this.role = role;
        this.Hs = Collections.unmodifiableMap(Hbar.entrySet().stream().collect(
                Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new)));
    }

    @Override
    public EAHandlersType infer() {
        EATypeFactory f = EATypeFactory.factory;
        List<EAValType> As = this.Hs.values().stream()
                .map(x -> x.svarType).distinct().collect(Collectors.toList());
        if (As.size() != 1) {
            throw new RuntimeException("Inconsistent state types: " + this);
        }
        LinkedHashMap<Op, EAPPair<EAValType, EALType>> cases =
                this.Hs.entrySet().stream().collect(Collectors.toMap(
                        Map.Entry::getKey,
                        x -> {
                            EAPHandler v = x.getValue();
                            return new EAPPair<>(v.varType, v.pre);
                        },
                        (x, y) -> null,
                        LinkedHashMap::new
                ));
        return f.val.handlers(f.local.in(this.role, cases), As.get(0));
    }

    @Override
    public boolean canBeta() {
        return false;
    }

    @Override
    public EAPExpr beta() {
        throw new RuntimeException("Stuck: " + this);
    }

    @Override
    public EAValType type(Gamma gamma) {
        LinkedHashMap<Op, EAPPair<EAValType, EALType>> cases = new LinkedHashMap<>();
        EAValType A = this.Hs.values().iterator().next().svarType;  // Syntactically non-empty
        for (Map.Entry<Op, EAPHandler> e : this.Hs.entrySet()) {
            Op k = e.getKey();
            EAPHandler v = e.getValue();
            if (!A.equals(v.svarType)) {
                throw new RuntimeException("Inconsistent state types: " + this);
            }
            v.type(gamma);
            cases.put(k, new EAPPair<>(v.varType, v.pre));
        }
        EALInType in = EALTypeFactory.factory.in(this.role, cases);
        return EAValTypeFactory.factory.handlers(in, A);
    }

    /* Aux */

    @Override
    public EAPHandlers subs(@NotNull Map<EAPVar, EAPExpr> m) {
        LinkedHashMap<Op, EAPHandler> Hs = new LinkedHashMap<>();
        for (Map.Entry<Op, EAPHandler> e : this.Hs.entrySet()) {
            Map<EAPVar, EAPExpr> m1 = new HashMap<>(m);
            Op k = e.getKey();
            Hs.put(k, e.getValue().subs(m));
        }
        return EAPFactory.factory.handlers(this.role, Hs);
    }

    @Override
    public EAPExpr fsubs(@NotNull Map<EAPFuncName, EAPRec> m) {
        LinkedHashMap<Op, EAPHandler> Hs = new LinkedHashMap<>();
        for (Map.Entry<Op, EAPHandler> e : this.Hs.entrySet()) {
            Map<EAPFuncName, EAPRec> m1 = new HashMap<>(m);
            Op k = e.getKey();
            Hs.put(k, e.getValue().fsubs(m));
        }
        return EAPFactory.factory.handlers(this.role, Hs);
    }

    @Override
    public Set<EAPVar> getFreeVars() {
        Set<EAPVar> res = this.Hs.values().stream()
                .flatMap(x -> x.getFreeVars().stream())
                .collect(Collectors.toCollection(HashSet::new));
        return res;
    }

    @Override
    public String toString() {
        return "handler " + this.role + " { "
                + this.Hs.entrySet().stream()
                .map(x -> x.getValue().toString())
                .collect(Collectors.joining(", "))
                + " }";
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
