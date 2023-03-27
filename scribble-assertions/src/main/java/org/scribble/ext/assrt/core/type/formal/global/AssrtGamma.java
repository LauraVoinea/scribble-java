package org.scribble.ext.assrt.core.type.formal.global;

import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtTrueFormula;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.util.AssrtUtil;
import org.scribble.ext.assrt.util.Triple;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class AssrtGamma {

    // Invar: nohat.keySet(), hat.keySet() disjoint
    public final Map<AssrtVar, Triple<Set<Role>, DataName, AssrtBFormula>> nohat;
    public final Map<AssrtVar, Triple<Set<Role>, DataName, AssrtBFormula>> hat;  // "Future" knowledge, due to global context reduction when no causal ordering

    //public final Set<AssrtBFormula> ass;  // for all nohat and hat  // HACK "cnf", cf. canAdd idemp needs refinement to be syntactically exact

    public AssrtGamma() {
        this(new LinkedHashMap<>(), new LinkedHashMap<>());
    }

    public AssrtGamma(
            LinkedHashMap<AssrtVar, Triple<Set<Role>, DataName, AssrtBFormula>> nohat,
            LinkedHashMap<AssrtVar, Triple<Set<Role>, DataName, AssrtBFormula>> hat) {
        this.nohat = Collections.unmodifiableMap(new LinkedHashMap<>(nohat));  // TODO ensure Set<Role> immut
        this.hat = Collections.unmodifiableMap(new LinkedHashMap<>(hat));
    }

    /*// TODO refactor
    public boolean canAddNoHat(AssrtVar v, Set<Role> rs, DataName t, AssrtBFormula ass) {
        if (this.nohat.containsKey(v)) {
            if (this.hat.containsKey(v)) {  // Cf. invar
                return false;
            } else {
                Triple<Set<Role>, DataName, AssrtBFormula> p = this.nohat.get(v);
                return p.left.equals(rs) && p.middle.equals(t) && p.right.equals(ass);
            }
        } else if (this.hat.containsKey(v)) {  // !nohat.containsKey(v)
            Triple<Set<Role>, DataName, AssrtBFormula> p = this.hat.get(v);
            return p.left.equals(rs) && p.middle.equals(t) && p.right.equals(ass);
        } else {
            return true;
        }
    }*/

    // Disjoint add
    public Optional<AssrtGamma> addNohat(AssrtVar v, Set<Role> rs, DataName t, AssrtBFormula ass) {
        if (this.nohat.containsKey(v)) {
            if (this.hat.containsKey(v)) {  // Cf. invar
                return Optional.empty();
            } else {
                Triple<Set<Role>, DataName, AssrtBFormula> p = this.nohat.get(v);
                return p.left.equals(rs) && p.middle.equals(t) && p.right.equals(ass)
                        ? Optional.of(this)
                        : Optional.empty();
            }
        } else if (this.hat.containsKey(v)) {  // !nohat.containsKey(v)
            Triple<Set<Role>, DataName, AssrtBFormula> p = this.hat.get(v);
            if (p.left.equals(rs) && p.middle.equals(t) && p.right.equals(ass)) {
                LinkedHashMap<AssrtVar, Triple<Set<Role>, DataName, AssrtBFormula>> nohat =
                        new LinkedHashMap<>(this.nohat);
                nohat.put(v, new Triple<>(rs, t, ass));
                LinkedHashMap<AssrtVar, Triple<Set<Role>, DataName, AssrtBFormula>> hat =
                        new LinkedHashMap<>(this.hat);
                hat.remove(v);
                return Optional.of(new AssrtGamma(nohat, hat));  // TODO optimise backing
            } else {
                return Optional.empty();
            }
        } else {
            LinkedHashMap<AssrtVar, Triple<Set<Role>, DataName, AssrtBFormula>> nohat =
                    new LinkedHashMap<>(this.nohat);
            nohat.put(v, new Triple<>(rs, t, ass));
            return Optional.of(new AssrtGamma(nohat, new LinkedHashMap<>(this.hat)));
        }
    }

    /*public boolean canAddHat(AssrtVar v, Set<Role> rs, DataName t, AssrtBFormula ass) {
        if (this.nohat.containsKey(v)) {
            return false;
        } else if (this.hat.containsKey(v)) {  // !nohat.containsKey(v)
            Triple<Set<Role>, DataName, AssrtBFormula> p = this.hat.get(v);
            return p.left.equals(rs) && p.middle.equals(t) && p.right.equals(ass);
        } else {
            return true;
        }
    }*/

    public Optional<AssrtGamma> addHat(AssrtVar v, Set<Role> rs, DataName t, AssrtBFormula ass) {
        if (this.nohat.containsKey(v)) {
            return Optional.empty();
        } else if (this.hat.containsKey(v)) {  // !nohat.containsKey(v)
            Triple<Set<Role>, DataName, AssrtBFormula> p = this.hat.get(v);
            return p.left.equals(rs) && p.middle.equals(t) && p.right.equals(ass)
                    ? Optional.of(this)
                    : Optional.empty();
        } else {
            LinkedHashMap<AssrtVar, Triple<Set<Role>, DataName, AssrtBFormula>> hat =
                    new LinkedHashMap<>(this.hat);
            hat.put(v, new Triple<>(rs, t, ass));
            return Optional.of(new AssrtGamma(new LinkedHashMap<>(this.nohat), hat));
        }
    }

    public Optional<AssrtGamma> addAll(AssrtGamma lam) {
        Optional<AssrtGamma> res = Optional.of(this);
        for (Map.Entry<AssrtVar, Triple<Set<Role>, DataName, AssrtBFormula>>
                e : lam.nohat.entrySet()) {
            Triple<Set<Role>, DataName, AssrtBFormula> p = e.getValue();
            res = res.flatMap(x -> x.addNohat(e.getKey(), p.left, p.middle, p.right));
        }
        for (Map.Entry<AssrtVar, Triple<Set<Role>, DataName, AssrtBFormula>>
                e : lam.hat.entrySet()) {
            Triple<Set<Role>, DataName, AssrtBFormula> p = e.getValue();
            res = res.flatMap(x -> x.addHat(e.getKey(), p.left, p.middle, p.right));
        }
        return res;
    }

    @Override
    public String toString() {
        return "({" +
                this.nohat.entrySet().stream()
                        .map(x -> x.getKey() + "=" + AssrtUtil.tripleToString(x.getValue()))
                        .collect(Collectors.joining(", ")) +
                "}, {" +
                this.hat.entrySet().stream()
                        .map(x -> x.getKey() + "=" + AssrtUtil.tripleToString(x.getValue()))
                        .collect(Collectors.joining(", ")) +
                "})";
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!(obj instanceof AssrtGamma)) {
            return false;
        }
        AssrtGamma cast = (AssrtGamma) obj;
        return this.nohat.equals(cast.nohat) && this.hat.equals(cast.hat);
    }

    @Override
    public int hashCode() {
        int hash = 7643;
        hash = 31 * hash + this.nohat.hashCode();
        hash = 31 * hash + this.hat.hashCode();
        return hash;
    }
}
