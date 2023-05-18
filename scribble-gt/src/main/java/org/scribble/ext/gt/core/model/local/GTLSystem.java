package org.scribble.ext.gt.core.model.local;

import org.jetbrains.annotations.Unmodifiable;
import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.action.GTEAction;
import org.scribble.ext.gt.core.model.local.action.GTENewTimeout;
import org.scribble.ext.gt.core.model.local.action.GTESend;
import org.scribble.ext.gt.core.type.session.local.GTLMixedChoice;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.util.Either;
import org.scribble.ext.gt.util.Tree;
import org.scribble.ext.gt.util.Triple;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

public class GTLSystem {

    public final Map<Role, GTLConfig> configs;

    public GTLSystem(Map<Role, GTLConfig> configs) {
        this.configs = Collections.unmodifiableMap(new HashMap<>(configs));
    }

    public Map<Role, LinkedHashSet<EAction<DynamicActionKind>>> getActs(GTEModelFactory mf) {
        return this.configs.entrySet().stream().collect(Collectors.toMap(
                Map.Entry::getKey,
                x -> x.getValue().getActs(mf)
        ));
    }

    public Either<Exception, Pair<GTLSystem, Tree<String>>> step(
            Set<Op> com, Role self, EAction<DynamicActionKind> a) {
        if (a instanceof GTENewTimeout) {  // !!! N.B. self is Role.EMPTY_ROLE
            // ...tau? (skip?) -- XXX then what is "projection" relation between G/L ?
            // ...find the one (or more?) guy(s) that can locally do new-timeout -- then do the rest implicitly when reach?
            throw new RuntimeException("TODO: " + self + " ,, " + a);
        } else {
            if (!(this.configs.containsKey(self))) {
                throw new RuntimeException("Unkown role: " + self);
            }
            Either<Exception, Pair<GTLConfig, Tree<String>>> step =
                    this.configs.get(self).step(com, a);
            if (step.isLeft()) {
                return Either.left(step.getLeft());
            }
            Pair<GTLConfig, Tree<String>> get = step.getRight();
            HashMap<Role, GTLConfig> tmp = new HashMap<>(this.configs);
            tmp.put(self, get.left);
            if (a instanceof GTESend) {
                GTESend cast = (GTESend) a;
                tmp.put(cast.peer, this.configs.get(cast.peer).enqueueMessage(self, cast));
            }
            return Either.right(Pair.of(new GTLSystem(tmp), get.right));
        }
    }

    // TODO factor out with above
    public Either<Exception, Pair<GTLSystem, Tree<String>>> weakStep(
            Set<Op> com, Role self, EAction<DynamicActionKind> a) {
        if (a instanceof GTENewTimeout) {  // !!! N.B. self is Role.EMPTY_ROLE
            throw new RuntimeException("Invalid for weak: " + self + " ,, " + a);
        } else {
            if (!(this.configs.containsKey(self))) {
                throw new RuntimeException("Unkown role: " + self);
            }
            Either<Exception, Pair<GTLConfig, Tree<String>>> step =
                    this.configs.get(self).weakStep(com, a);
            if (step.isLeft()) {
                return Either.left(step.getLeft());
            }
            Pair<GTLConfig, Tree<String>> get = step.getRight();
            HashMap<Role, GTLConfig> tmp = new HashMap<>(this.configs);
            tmp.put(self, get.left);
            if (a instanceof GTESend) {
                GTESend cast = (GTESend) a;
                tmp.put(cast.peer, this.configs.get(cast.peer).enqueueMessage(self, cast));
            }

            // TODO refactor
            for (Role r : this.configs.keySet()) {
                if (r.equals(self)) {
                    continue;
                }
                GTLConfig cfg = tmp.get(r);
                GTLType t = cfg.type;
                if (t instanceof GTLMixedChoice) {
                    Either<Exception, Pair<GTLConfig, Tree<String>>> step1 =
                            cfg.step(com, cfg.getActs((GTEModelFactory) GTEModelFactoryImpl.FACTORY.local).iterator().next());
                    if (step1.isLeft()) {
                        throw new RuntimeException(step1.getLeft());
                    }
                    cfg = step1.getRight().left;
                    t = cfg.type;
                }
                tmp.put(r, cfg);
            }

            return Either.right(Pair.of(new GTLSystem(tmp), get.right));
        }
    }

    @Override
    public String toString() {
        return this.configs.toString();
    }

    /* ... */

    @Override
    public int hashCode() {
        int hash = 49123;
        hash = 31 * hash + this.configs.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || !(obj instanceof GTLSystem)) return false;
        GTLSystem them = (GTLSystem) obj;
        return this.configs.equals(them.configs);
    }
}
