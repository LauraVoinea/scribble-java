package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.global.action.GTSAction;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.core.type.session.local.GTLTypeFactory;
import org.scribble.ext.gt.util.*;
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

    /* ... */

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
    public Optional<Pair<? extends GTLType, Sigma>> project(Set<Role> rs, Role r, int c, int n) {
        GTLTypeFactory lf = GTLTypeFactory.FACTORY;
        return this.body.project(rs, r, c, n).map(x ->
                x.left.equals(this.var)
                        ? Pair.of(lf.end(), new Sigma(rs))
                        : Pair.of(lf.recursion(this.var, x.left), x.right)
        );
    }

    @Override
    public Optional<Theta> projectTheta(Set<Integer> cs, Role r) {
        return Optional.of(new Theta(cs));
    }

    /* ... */

    @Override
    public Either<Exception, Triple<Theta, GTGType, Tree<String>>> step(
            Theta theta, SAction<DynamicActionKind> a, int c, int n) {
        Either<Exception, Triple<Theta, GTGType, Tree<String>>> step =
                unfoldAllOnce().step(theta, a, c, n);  // !!! cf. [Rec], unfold-subs after step
        return step.mapRight(x -> Triple.of(x.left, x.mid, Tree.of(
                toStepJudgeString(
                        "[Rec_" + this.var + "]",  // HACK for bounding execution
                        c, n, theta, this, (GTSAction) a, x.left, x.mid),
                x.right)));
    }

    @Override
    public LinkedHashSet<SAction<DynamicActionKind>>
    getActs(GTSModelFactory mf, Theta theta, Set<Role> blocked, int c, int n) {
        return this.body.getActs(mf, theta, blocked, c, n);
    }

    /* ... */

    @Override
    public Either<Exception, Triple<Theta, GTGType, Tree<String>>> weakStep(
            Theta theta, SAction<DynamicActionKind> a, int c, int n) {
        return step(theta, a, c, n);
    }

    @Override
    public LinkedHashSet<SAction<DynamicActionKind>> getWeakActs(
            GTSModelFactory mf, Theta theta, Set<Role> blocked, int c, int n) {
        return getActs(mf, theta, blocked, c, n);
    }

    /* ... */

    @Override
    public Set<Op> getCommittingTop(Set<Role> com) {
        return this.body.getCommittingTop();
    }

    @Override
    public Set<Op> getCommittingLeft(Role obs, Set<Role> com) {
        return this.body.getCommittingLeft(obs, com);
    }

    @Override
    public Set<Op> getCommittingRight(Role obs, Set<Role> com) {
        return this.body.getCommittingRight(obs, com);
    }

    @Override
    public Pair<Set<Op>, Map<Integer, Pair<Set<Op>, Set<Op>>>> getLabels() {
        return this.body.getLabels();
    }

    /* Aux */

    @Override
    public GTGRecursion subs(Map<RecVar, GTGType> subs) {
        if (subs.containsKey(this.var)) {
            return this;
        }
        return new GTGRecursion(this.var, this.body.subs(subs));
    }

    @Override
    public GTGType unfoldAllOnce() {
        return this.body.subs(GTUtil.mapOf(this.var, this)).unfoldAllOnce();
    }

    @Override
    public Set<Integer> getTimeoutIds() {
        return this.body.getTimeoutIds();
    }


    @Override
    public Set<Op> getOps() {
        return this.body.getOps();
    }

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
        if (this == obj) { return true; }
        if (obj == null || !(obj instanceof GTGRecursion)) { return false; }
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
