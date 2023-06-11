package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.GTEModelFactory;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.model.local.action.GTEAction;
import org.scribble.ext.gt.core.type.session.global.GTGType;
import org.scribble.ext.gt.util.*;

import java.util.*;

public class GTLRecursion implements GTLType {

    private final GTLTypeFactory fact = GTLTypeFactory.FACTORY;

    public final RecVar var;
    public final GTLType body;

    protected GTLRecursion(RecVar var, GTLType body) {
        this.var = var;
        this.body = body;
    }

    @Override
    public Optional<? extends GTLType> merge(GTLType t) {
        if (!(t instanceof GTLRecursion)) {
            return Optional.empty();
        }
        GTLRecursion cast = (GTLRecursion) t;
        if (this.var.equals(cast.var)) {
            Optional<? extends GTLType> merge = this.body.merge(cast.body);
            return merge.map(x -> this.fact.recursion(this.var, x));
        } else {
            return Optional.empty();
        }
    }

    /* ... */

    @Override
    public LinkedHashSet<EAction<DynamicActionKind>> getActs(
            GTEModelFactory mf, Role self, Set<Role> blocked, Sigma sigma, Theta theta, int c, int n) {
        return unfoldAllOnce().getActs(mf, self, blocked, sigma, theta, c, n);
    }

    @Override
    public Either<Exception, Quad<GTLType, Sigma, Theta, Tree<String>>> step(
            Set<Op> com, Role self, EAction<DynamicActionKind> a, Sigma sigma, Theta theta, int c, int n) {
        Either<Exception, Quad<GTLType, Sigma, Theta, Tree<String>>> step =
                unfoldAllOnce().step(com, self, a, sigma, theta, c, n);
        return step.mapRight(x -> Quad.of(x.fst, x.snd, x.thrd, Tree.of(
                toStepJudgeString("[Rec]", c, n, theta, this, sigma,
                        (GTEAction) a, x.thrd, x.fst, x.snd),
                x.frth)));
    }

    /* ... */

    @Override
    public LinkedHashSet<EAction<DynamicActionKind>> getWeakActs(
            GTEModelFactory mf, Set<Op> com, Role self, Set<Role> blocked, Sigma sigma, Theta theta, int c, int n) {
        return getActs(mf, self, blocked, sigma, theta, c, n);
    }

    @Override
    public Either<Exception, Quad<GTLType, Sigma, Theta, Tree<String>>> weakStep(
            Set<Op> com, Role self, EAction<DynamicActionKind> a, Sigma sigma, Theta theta, int c, int n) {
        return step(com, self, a, sigma, theta, c, n);
    }

    /* Aux */

    @Override
    public Map<Integer, Integer> getActive(Theta theta) {
        return GTUtil.mapOf();
    }

    @Override
    public GTLType subs(RecVar rv, GTLType t) {
        if (rv.equals(this.var)) {
            return this;
        }
        return new GTLRecursion(this.var, this.body.subs(rv, t));
    }

    @Override
    public GTLType unfoldAllOnce() {
        return this.body.subs(this.var, this).unfoldAllOnce();
    }

    @Override
    public String toString() {
        return ConsoleColors.toRecString("mu " + this.var + "." + this.body);
    }

    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTLType.REC_HASH;
        hash = 31 * hash + this.var.hashCode();
        hash = 31 * hash + this.body.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || !(obj instanceof GTLRecursion)) return false;
        GTLRecursion them = (GTLRecursion) obj;
        return them.canEquals(this)
                && this.var.equals(them.var)
                && this.body.equals(them.body);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTLRecursion;
    }
}
