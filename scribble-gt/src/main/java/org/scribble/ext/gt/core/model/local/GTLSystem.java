package org.scribble.ext.gt.core.model.local;

import org.jetbrains.annotations.Unmodifiable;
import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.action.GTEAction;
import org.scribble.ext.gt.core.model.local.action.GTENewTimeout;
import org.scribble.ext.gt.core.model.local.action.GTESend;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.util.Triple;

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

    public Optional<GTLSystem> step(Role self, EAction<DynamicActionKind> a) {
        if (a instanceof GTENewTimeout) {
            // !!! N.B. r is Role.EMPTY_ROLE
            // ...tau? (skip?) -- XXX then what is "projection" relation between G/L ?
            // ...find the one (or more?) guy(s) that can locally do new-timeout -- then do the rest implicitly when reach?
            throw new RuntimeException("TODO: " + a);
        } else {
            if (!(this.configs.containsKey(self))) {
                throw new RuntimeException("Unkown role: " + self);
            }
            Optional<GTLConfig> step = this.configs.get(self).step(a);
            if (!step.isPresent()) {
                return Optional.empty();
            }
            GTLConfig get = step.get();
            HashMap<Role, GTLConfig> tmp = new HashMap<>(this.configs);
            tmp.put(self, get);
            if (a instanceof GTESend) {
                GTESend cast = (GTESend) a;
                tmp.put(cast.peer, this.configs.get(cast.peer).enqueueMessage(self, cast));
            }
            return Optional.of(new GTLSystem(tmp));
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
