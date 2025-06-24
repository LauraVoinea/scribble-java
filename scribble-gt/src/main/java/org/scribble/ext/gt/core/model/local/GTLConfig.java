package org.scribble.ext.gt.core.model.local;

import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.util.GTUtil;
import org.scribble.util.Pair;

import java.util.Map;

public class GTLConfig {

    public final Role self;
    public final GTLType type;
    public final Sigma sigma;
    public final Theta theta;

    public final Map<Pair<Integer, Integer>, Discard> discard;  // key is c, n

    public GTLConfig(Role self, GTLType type, Sigma sigma, Theta theta,
                     Map<Pair<Integer, Integer>, Discard> discard) {
        this.self = self;
        this.type = type;
        this.sigma = sigma;
        this.theta = theta;
        this.discard = GTUtil.copyOf(discard);
    }


    @Override
    public String toString() {
        return "<" + this.self + ", " + this.type + ", " + this.sigma + ", " +
                this.theta + ", " + this.discard + ">";
    }

    /* ... */

    @Override
    public int hashCode() {
        int hash = 49121;
        hash = 31 * hash + this.self.hashCode();
        hash = 31 * hash + this.type.hashCode();
        hash = 31 * hash + this.sigma.hashCode();
        hash = 31 * hash + this.theta.hashCode();
        hash = 31 * hash + this.discard.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) { return true; }
        if (obj == null || !(obj instanceof GTLConfig)) { return false; }
        GTLConfig them = (GTLConfig) obj;
        return this.self.equals(them.self)
                && this.type.equals(them.type)
                && this.sigma.equals(them.sigma)
                && this.theta.equals(them.theta)
                && this.discard.equals(them.discard);
    }
}
