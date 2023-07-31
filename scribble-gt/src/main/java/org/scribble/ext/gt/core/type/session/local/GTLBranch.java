package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.Discard;
import org.scribble.ext.gt.core.model.local.GTEModelFactory;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.model.local.action.GTEAction;
import org.scribble.ext.gt.core.model.local.action.GTERecv;
import org.scribble.ext.gt.core.model.local.action.GTESend;
import org.scribble.ext.gt.util.*;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

// !!! FIXME naming "interaction" vs. "choice" (in other places)
public class GTLBranch implements GTLType {

    private final GTLTypeFactory fact = GTLTypeFactory.FACTORY;

    public final Role src;  // Sender
    public final Map<Op, GTLType> cases;  // Pre: Unmodifiable

    protected GTLBranch(Role src, LinkedHashMap<Op, GTLType> cases) {
        this.src = src;
        this.cases = Collections.unmodifiableMap(cases.entrySet().stream().collect(
                Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new)));
    }

    @Override
    public Optional<? extends GTLType> merge(GTLType t) {
        if (!(t instanceof GTLBranch)) {
            return Optional.empty();
        }
        GTLBranch cast = (GTLBranch) t;
        if (!this.src.equals(cast.src)) {
            return Optional.empty();
        }
        LinkedHashMap<Op, GTLType> tmp = new LinkedHashMap<>();
        Iterator<Op> it = Stream.of(this.cases.keySet(), cast.cases.keySet()).flatMap(Collection::stream).iterator();
        while (it.hasNext()) {
            Op x = it.next();
            if (this.cases.keySet().contains(x)) {  // Would be nice if get returned Optional... can map empty directly
                if (cast.cases.keySet().contains(x)) {
                    // case x in both branches: !!! currently recursively merging but could just simplify to equality
                    Optional<? extends GTLType> opt = this.cases.get(x).merge(cast.cases.get(x));
                    if (!opt.isPresent()) {
                        return Optional.empty();
                    }
                    tmp.put(x, opt.get());
                } else {
                    tmp.put(x, this.cases.get(x));
                }
            } else { //if (cast.cases.keySet().contains(x)) {
                tmp.put(x, cast.cases.get(x));
            }
        }
        return Optional.of(this.fact.branch(this.src, tmp));
    }

    /* ... */

    @Override
    public LinkedHashSet<EAction<DynamicActionKind>> getActs(
            GTEModelFactory mf, Role self, Set<Role> blocked, Sigma sigma, Theta theta, int c, int n) {
        Optional<GTESend<DynamicActionKind>> first = sigma.map.get(this.src)
                .stream().filter(x -> x.c == c && x.n == n).findFirst();
        if (first.isPresent()) {
            GTESend<DynamicActionKind> m = first.get();
            return Stream.of(m.toDynamicDual(this.src))
                    .collect(Collectors.toCollection(LinkedHashSet::new));
        } else {
            return new LinkedHashSet<>();
        }
    }

    @Override
    public Either<Exception, Pair<Quad<GTLType, Sigma, Theta, Tree<String>>,
            Map<Pair<Integer, Integer>, Discard>>> step(
            Set<Op> com, Role self, EAction<DynamicActionKind> a, Sigma sigma, Theta theta, int c, int n) {

        if (!(a instanceof GTERecv<?>) || !sigma.map.containsKey(a.peer)) {
            return Either.left(newStuck(c, n, theta, this, (GTEAction) a));
        }
        GTERecv<DynamicActionKind> cast = (GTERecv<DynamicActionKind>) a;
        GTESend<DynamicActionKind> m = cast.toDynamicDual(self);
        if (!sigma.map.get(a.peer).contains(m)) {
            return Either.left(newStuck(c, n, theta, this, (GTEAction) a));
        }

        if (!a.peer.equals(this.src) || !this.cases.keySet().contains(a.mid)  // TODO check payload?
                || cast.c != c || cast.n != n) {
            //System.out.println("99999999: " + !a.peer.equals(this.src) + " ,, " + !this.cases.keySet().contains(a.mid) + " ,, " + (cast.c != c) + " .. " + cast.c + " .. " + c + " ,, " + (cast.n != n));
            return Either.left(newStuck(c, n, theta, this, (GTEAction) a));
        }
        boolean[] found = {false};
        List<GTESend<DynamicActionKind>> tmp = sigma.map.get(a.peer).stream().filter(x -> {
            if (!found[0] && x.equals(m)) {
                found[0] = true;
                return false;
            }
            return true;
        }).collect(Collectors.toList());
        Map<Role, List<GTESend<DynamicActionKind>>> map = new HashMap<>(sigma.map);
        map.put(this.src, tmp);
        Sigma sigma1 = new Sigma(map);
        GTLType succ = this.cases.get(a.mid);
        return Either.right(Pair.of(
                Quad.of(succ, sigma1, theta, Tree.of(
                        toStepJudgeString("[Rcv]", c, n, theta, this, sigma,
                                (GTEAction) a, theta, succ, sigma1)
                )),
                GTUtil.mapOf()
        ));
    }

    /* ... */

    @Override
    public LinkedHashSet<EAction<DynamicActionKind>> getWeakActs(
            GTEModelFactory mf, Set<Op> com, Role self, Set<Role> blocked, Sigma sigma, Theta theta, int c, int n) {
        return getActs(mf, self, blocked, sigma, theta, c, n);
    }

    @Override
    public Either<Exception, Pair<Quad<GTLType, Sigma, Theta, Tree<String>>,
            Map<Pair<Integer, Integer>, Discard>>> weakStep(
            Set<Op> com, Role self, EAction<DynamicActionKind> a, Sigma sigma, Theta theta, int c, int n) {
        return step(com, self, a, sigma, theta, c, n);
    }

    /* Aux */

    @Override
    public Map<Integer, Integer> getActive(Theta theta) {
        return this.cases.values().stream()
                .flatMap(x -> x.getActive(theta).entrySet().stream())
                .collect(Collectors.toMap(
                        Map.Entry::getKey,
                        Map.Entry::getValue,
                        (x, y) -> x < y ? x : y,
                        LinkedHashMap::new
                ));
    }

    @Override
    public GTLBranch subs(RecVar rv, GTLType t) {
        LinkedHashMap<Op, GTLType> cases = this.cases.entrySet().stream()
                .collect(Collectors.toMap(
                        Map.Entry::getKey,
                        x -> x.getValue().subs(rv, t),
                        (x, y) -> null,
                        LinkedHashMap::new
                ));
        return this.fact.branch(this.src, cases);
    }

    @Override
    public GTLBranch unfoldAllOnce() {
        return this;
    }

    @Override
    public String toString() {
        return this.src + "& {"
                + this.cases.entrySet().stream()
                .map(e -> e.getKey() + "." + e.getValue())
                .collect(Collectors.joining(", "))
                + "}";
    }

    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTLType.BRANCH_HASH;
        hash = 31 * hash + this.src.hashCode();
        hash = 31 * hash + this.cases.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) { return true; }
        if (obj == null || !(obj instanceof GTLBranch)) { return false; }
        GTLBranch them = (GTLBranch) obj;
        return them.canEquals(this)
                && this.src.equals(them.src)
                && this.cases.equals(them.cases);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTLBranch;
    }
}
