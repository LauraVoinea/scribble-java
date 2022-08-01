package org.scribble.ext.ea.core.config;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.process.*;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.Delta;
import org.scribble.ext.ea.core.type.session.local.EALEndType;
import org.scribble.ext.ea.core.type.session.local.EALInType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAUnitType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.util.EAPPair;
import org.scribble.ext.ea.util.EATriple;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

// cf. T-Actor
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

    public void type(Gamma gamma, Delta delta) {
        //throw new RuntimeException("TODO");
        LinkedHashMap<Pair<EAPSid, Role>, EALType> tmp = new LinkedHashMap<>();
        if (this.T instanceof EAPActiveThread) { // !!! CHECKME
            EAPActiveThread at = (EAPActiveThread)  this.T;
            Pair<EAPSid, Role> k = new Pair<>(at.sid, at.role);
            tmp.put(k, delta.map.get(k));
        }
        Delta delta1 = new Delta(tmp);
        this.T.type(gamma, delta1);  // !!! TODO split delta_1, delta_2 ?

        tmp = new LinkedHashMap<>(delta.map);
        if (this.T instanceof EAPActiveThread) { // !!! CHECKME
            tmp.remove(delta1.map.keySet().iterator().next());
        }
        Delta delta2 = new Delta(tmp);
        typeSigma(gamma, delta2);
    }

    protected void typeSigma(Gamma gamma, Delta delta) {
        if (delta.map.size() != this.sigma.size()) {
            throw new RuntimeException("Invalid delta: " + delta + " |- " + this.sigma);
        }

        for (Map.Entry<Pair<EAPSid, Role>, EAPHandlers> e : this.sigma.entrySet()) {
            Pair<EAPSid, Role> k = e.getKey();
            EALType T = delta.map.get(k);
            if (!(T instanceof EALInType)) {
                throw new RuntimeException("Invalid handler type: " + e + " : " + T);
            }
            EALInType cast = (EALInType) T;
            if (!cast.peer.equals(k.right)) {
                throw new RuntimeException("Invalid handler type peer: " + e + " : " + T);
            }
            EAPHandlers h = this.sigma.get(k);
            if (!h.role.equals(k.right)) {
                throw new RuntimeException("Invalid handler peer: " + e + " : " + T);
            }
            if (!cast.cases.keySet().equals(h.Hs.keySet())) {
                throw new RuntimeException("Bad handler set: " + cast.cases + " |> " + h.Hs);
            }
            for (Map.Entry<Op, EATriple<EAPVar, EAValType, EAPExpr>> x : h.Hs.entrySet()) {
                Op op = x.getKey();
                EATriple<EAPVar, EAValType, EAPExpr> rhs = x.getValue();
                LinkedHashMap<EAName, EAValType> tmp = new LinkedHashMap<>(gamma.map);
                tmp.put(rhs.left, rhs.mid);
                Gamma gamma1 = new Gamma(tmp);
                Pair<EAValType, EALType> res = rhs.right.type(gamma1, cast.cases.get(op).right);
                if (!res.equals(new EAPPair<>(EAUnitType.UNIT, EALEndType.END))) {
                    throw new RuntimeException("Badly typed: " + rhs.right + " |> " + res);
                }
            }
        }
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
