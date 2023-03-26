package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.core.type.session.local.GTLTypeFactory;
import org.scribble.ext.gt.util.ConsoleColors;
import org.scribble.ext.gt.util.Triple;
import org.scribble.util.Pair;

import java.util.*;

public class GTGRecursion implements GTGType {

    //private final GTGTypeFactory fact = GTGTypeFactory.FACTORY;

    public final RecVar var;
    public final GTGType body;

    protected GTGRecursion(RecVar var, GTGType body) {
        this.var = var;
        this.body = body;
    }

    @Override
    public GTGType unfoldContext(Map<RecVar, GTGType> c) {
        if (c.containsKey(this.var)) {
            return this;
        }
        Map<RecVar, GTGType> nested = new HashMap<>(c);
        nested.put(this.var, this);
        return this.body.unfoldContext(nested);
    }

    @Override
    public Set<Integer> getTimeoutIds() {
        return this.body.getTimeoutIds();
    }

    @Override
    public Optional<Pair<? extends GTLType, Sigma>> project(Role r) {
        GTLTypeFactory lf = GTLTypeFactory.FACTORY;
        return this.body.project(r).map(x -> new Pair<>(
                x.left.equals(this.var) ? lf.end() : lf.recursion(this.var, x.left),
                x.right));
    }

    @Override
    public boolean isSinglePointed() {
        return this.body.isSinglePointed();
    }

    @Override
    public boolean isGood() {
        return this.body.isGood();
    }

    @Override
    public boolean isCoherent() {
        return this.body.isCoherent();
    }

    @Override
    public Optional<Triple<Theta, GTGType, String>> step(Theta theta, SAction<DynamicActionKind> a) {
        Optional<Triple<Theta, GTGType, String>> step = unfold().step(theta, a);  // !!! cf. [Rec], unfold-subs after step
        return step.map(x -> new Triple<>(x.left, x.mid, "[Rec_" + this.var + "]" + x.right));
    }

    @Override
    public LinkedHashSet<SAction<DynamicActionKind>>
    getActs(GTSModelFactory mf, Theta theta, Set<Role> blocked) {
        return this.body.getActs(mf, theta, blocked);
    }

    @Override
    public Set<Op> getOps() {
        return this.body.getOps();
    }

    /* Aux */

    @Override
    public String toString() {
        return ConsoleColors.toRecString("mu " + this.var + "." + this.body);
    }

    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTGType.REC_HASH;
        hash = 31 * hash + this.var.hashCode();
        hash = 31 * hash + this.body.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || !(obj instanceof GTGRecursion)) return false;
        GTGRecursion them = (GTGRecursion) obj;
        return them.canEquals(this)
                && this.var.equals(them.var)
                && this.body.equals(them.body);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTGRecursion;
    }
}
