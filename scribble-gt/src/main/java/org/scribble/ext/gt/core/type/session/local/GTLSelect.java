package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.GTEModelFactory;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.model.local.action.GTEAction;
import org.scribble.ext.gt.core.model.local.action.GTESend;
import org.scribble.ext.gt.util.Either;
import org.scribble.ext.gt.util.Quad;
import org.scribble.ext.gt.util.Tree;
import org.scribble.ext.gt.util.Triple;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
    public GTLSelect unfoldContext(Map<RecVar, GTLType> env) {
        LinkedHashMap<Op, GTLType> cases = this.cases.entrySet().stream()
                .collect(Collectors.toMap(
                        Map.Entry::getKey,
                        x -> x.getValue().unfoldContext(env),
                        (x, y) -> null,
                        LinkedHashMap::new
                ));
        return this.fact.select(this.dst, cases);
    }

    @Override
    public Optional<? extends GTLType> merge(GTLType t) {
        return this.equals(t) ? Optional.of(this) : Optional.empty();
    }

    @Override
    public Either<Exception, Quad<GTLType, Sigma, Theta, Tree<String>>> step(
            Role self, EAction<DynamicActionKind> a, Sigma sigma, Theta theta, int c, int n) {
        if (!(a instanceof GTESend<?>) || !sigma.map.containsKey(self)) {
            return Either.left(newStuck(c, n, theta, this, (GTEAction) a));
        }
        GTESend<DynamicActionKind> cast = (GTESend<DynamicActionKind>) a;
        if (!a.peer.equals(this.dst) || !this.cases.keySet().contains(a.mid)  // TODO check payload?
                || cast.c != c || cast.n != n) {
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
        return Either.right(Quad.of(succ, sigma1, theta, Tree.of(
                toStepJudgeString("[Snd]", c, n, theta, this, sigma,
                        (GTEAction) a, theta, succ, sigma1)
        )));
    }

    @Override
    public LinkedHashSet<EAction<DynamicActionKind>> getActs(
            GTEModelFactory mf, Role self, Set<Role> blocked, Sigma sigma, Theta theta, int c, int n) {
        if (!sigma.map.containsKey(self)) {
            throw new RuntimeException("Unknown sender: " + this);
        }
        return this.cases.entrySet().stream()
                .map(x -> mf.DynamicESend(this.dst, x.getKey(), Payload.EMPTY_PAYLOAD))  // FIXME pay
                .collect(Collectors.toCollection(LinkedHashSet::new));
    }

    /* Aux */

    @Override
    public String toString() {
        return this.dst + "+ {"
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
        if (this == obj) return true;
        if (obj == null || !(obj instanceof GTLSelect)) return false;
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
