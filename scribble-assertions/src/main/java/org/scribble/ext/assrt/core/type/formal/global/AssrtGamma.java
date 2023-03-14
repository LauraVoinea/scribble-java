package org.scribble.ext.assrt.core.type.formal.global;

import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.util.AssrtUtil;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

public class AssrtGamma {

    // Invar: nohat.keySet(), hat.keySet() disjoint
    public final Map<AssrtVar, Pair<Set<Role>, DataName>> nohat;  // TODO assertions -- cf. AssrtLambda
    public final Map<AssrtVar, Pair<Set<Role>, DataName>> hat;

    public AssrtGamma() {
        this(new LinkedHashMap<>(), new LinkedHashMap<>());
    }

    public AssrtGamma(
            LinkedHashMap<AssrtVar, Pair<Set<Role>, DataName>> nohat,
            LinkedHashMap<AssrtVar, Pair<Set<Role>, DataName>> hat) {
        this.nohat = Collections.unmodifiableMap(new LinkedHashMap<>(nohat));  // TODO ensure Set<Role> immut
        this.hat = Collections.unmodifiableMap(new LinkedHashMap<>(hat));
    }

    // TODO refactor
    public boolean canAddNoHat(AssrtVar v, Set<Role> rs, DataName t) {
        if (this.nohat.containsKey(v)) {
            if (this.hat.containsKey(v)) {  // Cf. invar
                return false;
            } else {
                Pair<Set<Role>, DataName> p = this.nohat.get(v);
                return p.left.equals(rs) && p.right.equals(t);
            }
        } else if (this.hat.containsKey(v)) {  // !nohat.containsKey(v)
            Pair<Set<Role>, DataName> p = this.hat.get(v);
            return p.left.equals(rs) && p.right.equals(t);
        } else {
            return true;
        }
    }

    // Disjoint add
    public Optional<AssrtGamma> addNohat(AssrtVar v, Set<Role> rs, DataName t) {
        if (this.nohat.containsKey(v)) {
            if (this.hat.containsKey(v)) {  // Cf. invar
                return Optional.empty();
            } else {
                Pair<Set<Role>, DataName> p = this.nohat.get(v);
                return p.left.equals(rs) && p.right.equals(t)
                        ? Optional.of(this)
                        : Optional.empty();
            }
        } else if (this.hat.containsKey(v)) {  // !nohat.containsKey(v)
            Pair<Set<Role>, DataName> p = this.hat.get(v);
            if (p.left.equals(rs) && p.right.equals(t)) {
                LinkedHashMap<AssrtVar, Pair<Set<Role>, DataName>> nohat =
                        new LinkedHashMap<>(this.nohat);
                nohat.put(v, new Pair<>(rs, t));
                LinkedHashMap<AssrtVar, Pair<Set<Role>, DataName>> hat =
                        new LinkedHashMap<>(this.hat);
                hat.remove(v);
                return Optional.of(new AssrtGamma(nohat, hat));
            } else {
                return Optional.empty();
            }
        } else {
            LinkedHashMap<AssrtVar, Pair<Set<Role>, DataName>> nohat =
                    new LinkedHashMap<>(this.nohat);
            nohat.put(v, new Pair<>(rs, t));
            return Optional.of(new AssrtGamma(nohat, new LinkedHashMap<>(this.hat)));
        }
    }

    public boolean canAddHat(AssrtVar v, Set<Role> rs, DataName t) {
        if (this.nohat.containsKey(v)) {
            return false;
        } else if (this.hat.containsKey(v)) {  // !nohat.containsKey(v)
            Pair<Set<Role>, DataName> p = this.hat.get(v);
            return p.left.equals(rs) && p.right.equals(t);
        } else {
            return true;
        }
    }

    public Optional<AssrtGamma> addHat(AssrtVar v, Set<Role> rs, DataName t) {
        if (this.nohat.containsKey(v)) {
            return Optional.empty();
        } else if (this.hat.containsKey(v)) {  // !nohat.containsKey(v)
            Pair<Set<Role>, DataName> p = this.hat.get(v);
            return p.left.equals(rs) && p.right.equals(t)
                    ? Optional.of(this)
                    : Optional.empty();
        } else {
            LinkedHashMap<AssrtVar, Pair<Set<Role>, DataName>> hat =
                    new LinkedHashMap<>(this.hat);
            hat.put(v, new Pair<>(rs, t));
            return Optional.of(new AssrtGamma(new LinkedHashMap<>(this.nohat), hat));
        }
    }

    public Optional<AssrtGamma> addAll(AssrtGamma lam) {
        Optional<AssrtGamma> res = Optional.of(this);
        for (Map.Entry<AssrtVar, Pair<Set<Role>, DataName>> e : lam.nohat.entrySet()) {
            Pair<Set<Role>, DataName> p = e.getValue();
            res = res.flatMap(x -> x.addNohat(e.getKey(), p.left, p.right));
        }
        for (Map.Entry<AssrtVar, Pair<Set<Role>, DataName>> e : lam.hat.entrySet()) {
            Pair<Set<Role>, DataName> p = e.getValue();
            res = res.flatMap(x -> x.addHat(e.getKey(), p.left, p.right));
        }
        return res;
    }

    @Override
    public String toString() {
        return "({" +
                this.nohat.entrySet().stream()
                        .map(x -> x.getKey() + "=" + AssrtUtil.pairToString(x.getValue()))
                        .collect(Collectors.joining(", ")) +
                "}, {" +
                this.hat.entrySet().stream()
                        .map(x -> x.getKey() + "=" + AssrtUtil.pairToString(x.getValue()))
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
