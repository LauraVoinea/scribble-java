package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.Discard;
import org.scribble.ext.gt.core.model.local.GTEModelFactory;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.model.local.action.GTEAction;
import org.scribble.ext.gt.core.model.local.action.GTESend;
import org.scribble.ext.gt.util.*;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

// !!! FIXME naming "interaction" vs. "choice" (in other places)
public class GTLSelect implements GTLType {

    private final GTLTypeFactory fact = GTLTypeFactory.FACTORY;

    public final Role dst;
    public final Map<Op, GTLType> cases;  // Pre: Unmodifiable

    protected GTLSelect(Role dst, LinkedHashMap<Op, GTLType> cases) {
        this.dst = dst;
        this.cases = Collections.unmodifiableMap(cases.entrySet().stream().collect(
                Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new)));
    }

    @Override
    public Optional<? extends GTLType> merge(GTLType t) {
        return this.equals(t) ? Optional.of(this) : Optional.empty();
    }

    /* ... */

    @Override
    //public LinkedHashSet<EAction<DynamicActionKind>> getActs(
    public LinkedHashMap<EAction<DynamicActionKind>, Set<RecVar>> getActs(
            GTEModelFactory mf, Role self, Set<Role> blocked, Sigma sigma, Theta theta, int c, int n) {
        /*if (!sigma.map.containsKey(self)) {  // XXX self is the sender, sigma is self/sender's buffer
            throw new RuntimeException("Unknown sender: " + this + " ,, " + sigma.map);
        }*/

        /*return this.cases.entrySet().stream()
                //.map(x -> mf.DynamicESend(this.dst, x.getKey(), Payload.EMPTY_PAYLOAD))  // FIXME pay
                .map(x -> mf.DynamicGTESend(this.dst, x.getKey(), Payload.EMPTY_PAYLOAD, c, n))  // FIXME pay
                .collect(Collectors.toCollection(LinkedHashSet::new));*/
        return this.cases.entrySet().stream()
                .map(x -> mf.DynamicGTESend(this.dst, x.getKey(), Payload.EMPTY_PAYLOAD, c, n))  // FIXME pay
                .collect(Collectors.toMap(
                        x -> x,
                        x -> Collections.emptySet(),
                        (x, y) -> x,
                        LinkedHashMap::new
                ));
    }

    @Override
    public Either<Exception, Pair<Quad<GTLType, Sigma, Theta, Tree<String>>,
            Map<Pair<Integer, Integer>, Discard>>> step(
            Set<Op> com, Role self, EAction<DynamicActionKind> a, Sigma sigma, Theta theta, int c, int n) {
        if (!(a instanceof GTESend<?>)) {
            return Either.left(newStuck(c, n, theta, this, (GTEAction) a));
        }
        GTESend<DynamicActionKind> cast = (GTESend<DynamicActionKind>) a;
        if (!a.peer.equals(this.dst) || !this.cases.containsKey(a.mid)  // TODO check payload?

                //|| cast.c != c
                || (cast.c != c && c != -1)  // FIXME HACK

                || cast.n != n) {
            return Either.left(newStuck(c, n, theta, this, (GTEAction) a));
        }
        /*Map<Role, List<GTESend<DynamicActionKind>>> map = new HashMap<>(sigma.map);
        List<GTESend<DynamicActionKind>> tmp = Stream.concat(
                sigma.map.get(self).stream(),
                Stream.of(cast)
        ).collect(Collectors.toList());
        map.put(self, tmp);
        Sigma sigma1 = new Sigma(map);*/
        Sigma sigma1 = sigma;
        GTLType succ = this.cases.get(a.mid);
        return Either.right(Pair.of(Quad.of(succ, sigma1, theta, Tree.of(
                        toStepJudgeString("[Snd]", c, n, theta, this, sigma,
                                (GTEAction) a, theta, succ, sigma1)  // !!! sender config only (not receiver), cf. GTLSystem.(weak)step
                )),
                GTUtil.mapOf()
        ));
    }

    /* ... */

    @Override
    public LinkedHashSet<EAction<DynamicActionKind>> getWeakActs(
            GTEModelFactory mf, Set<Op> com, Role self, Set<Role> blocked, Sigma sigma, Theta theta, int c, int n) {
        //return getActs(mf, self, blocked, sigma, theta, c, n);
        return new LinkedHashSet<>(getActs(mf, self, blocked, sigma, theta, c, n).keySet());
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
    public GTLSelect subs(RecVar rv, GTLType t) {
        LinkedHashMap<Op, GTLType> cases = this.cases.entrySet().stream()
                .collect(Collectors.toMap(
                        Map.Entry::getKey,
                        x -> x.getValue().subs(rv, t),
                        (x, y) -> null,
                        LinkedHashMap::new
                ));
        return this.fact.select(this.dst, cases);
    }

    @Override
    public GTLSelect unfoldAllOnce() {
        return this;
    }

    @Override
    public String toString() {
        return this.dst + (ConsoleColors.OLPLUS + "{")
                + this.cases.entrySet().stream()
                .map(e -> e.getKey() + "." + e.getValue())
                .collect(Collectors.joining(", "))
                + "}";
    }

    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTLType.SELECT_HASH;
        hash = 31 * hash + this.dst.hashCode();
        hash = 31 * hash + this.cases.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) { return true; }
        if (obj == null || !(obj instanceof GTLSelect)) { return false; }
        GTLSelect them = (GTLSelect) obj;
        return them.canEquals(this)
                && this.dst.equals(them.dst)
                && this.cases.equals(them.cases);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTLSelect;
    }
}
