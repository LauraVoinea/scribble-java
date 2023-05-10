package org.scribble.ext.gt.core.model.local;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.action.GTEAction;
import org.scribble.ext.gt.core.model.local.action.GTESend;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.util.Triple;

import java.util.*;

public class GTLConfig {

    public final Role self;
    public final GTLType type;
    public final Sigma sigma;
    public final Theta theta;

    public GTLConfig(Role self, GTLType type, Sigma sigma, Theta theta) {
        this.self = self;
        this.type = type;
        this.sigma = sigma;
        this.theta = theta;
    }

    public LinkedHashSet<EAction<DynamicActionKind>> getActs(GTEModelFactory mf) {
        return this.type.getActsTopLevel(mf, this.self, this.sigma, this.theta);
    }

    // n.b., GTESend only updates this local sender config -- use enqueueMessage to also update the receiver config
    public Optional<GTLConfig> step(EAction<DynamicActionKind> a) {
        if (!(a instanceof GTEAction)) {
            throw new RuntimeException("Shouldn't get in here: " + a);
        }
        Optional<Triple<GTLType, Sigma, Theta>> opt = this.type.stepTopLevel(this.self, a, this.sigma, this.theta);
        return opt.map(x -> new GTLConfig(this.self, x.left, x.mid, x.right));
    }

    public GTLConfig enqueueMessage(GTESend<DynamicActionKind> a) {
        Map<Role, List<GTESend<DynamicActionKind>>> map = new HashMap<>(this.sigma.map);
        List<GTESend<DynamicActionKind>> ms = new LinkedList<>(map.get(a.peer));
        ms.add(a);
        map.put(a.peer, ms);
        Sigma sigma = new Sigma(map);
        return new GTLConfig(this.self, this.type, sigma, this.theta);
    }

    /* ... */

    @Override
    public int hashCode() {
        int hash = 49121;
        hash = 31 * hash + this.self.hashCode();
        hash = 31 * hash + this.type.hashCode();
        hash = 31 * hash + this.sigma.hashCode();
        hash = 31 * hash + this.theta.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || !(obj instanceof GTLConfig)) return false;
        GTLConfig them = (GTLConfig) obj;
        return this.self.equals(them.self)
                && this.type.equals(them.type)
                && this.sigma.equals(them.sigma)
                && this.theta.equals(them.theta);
    }
}
