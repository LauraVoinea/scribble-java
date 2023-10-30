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
import org.scribble.ext.gt.util.Either;
import org.scribble.ext.gt.util.GTUtil;
import org.scribble.ext.gt.util.Tree;
import org.scribble.ext.gt.util.Triple;
import org.scribble.util.Pair;

import java.util.*;

// !!! No "fid"
public class GTGEnd implements GTGType {

    public static final GTGEnd END = new GTGEnd();

    protected GTGEnd() { }

    /* ... */

    @Override
    public boolean isSinglePointed() {
        return true;
    }

    @Override
    public boolean isGood() {
        return true;
    }

    /* ... */

    @Override
    public boolean isInitial() {
        return true;
    }

    @Override
    public boolean isInitialWellSet(Set<Integer> cs) {
        return true;
    }

    @Override
    public Map<Role, Set<Role>> getStrongDeps() {
        return GTUtil.mapOf();
    }

    @Override
    public boolean isSingleDecision(Set<Role> top, Theta theta) {
        return true;
    }

    @Override
    public boolean isClearTermination() {
        return true;
    }

    @Override
    public boolean isLeftCommitting(Set<Role> com, Set<Role> rem) {
        return rem.isEmpty();
    }

    @Override
    public boolean isLeftCommittingAux(Role obs, Set<Role> com, Set<Role> rem) {
        return rem.isEmpty();
    }

    /* ... */

    @Override
    public boolean isChoicePartip() {
        return true;
    }

    @Override
    public boolean isUniqueInstan(Set<Pair<Integer, Integer>> seen) {
        return true;
    }

    @Override
    public boolean isAwareCorollary(GTSModelFactory mf, Set<Role> top, Theta theta) {
        return true;
    }

    @Override
    public boolean isCoherent() {
        return true;
    }

    /* ... */
    
    /*@Override
    public Optional<Pair<? extends GTLType, Sigma>> project(Set<Role> rs, Role r) {
        return Optional.of(new Pair<>(GTLTypeFactory.FACTORY.end(), new Sigma(rs)));
    }*/

    @Override
    public Optional<Pair<? extends GTLType, Sigma>> project(Set<Role> topPeers, Role r, int c, int n) {
        //return project(rs, r);
        return Optional.of(new Pair<>(GTLTypeFactory.FACTORY.end(), new Sigma(topPeers)));
    }

    @Override
    public Optional<Theta> projectTheta(Set<Integer> cs, Role r) {
        return Optional.of(new Theta(cs));
    }

    /* ... */

    @Override
    public LinkedHashSet<SAction<DynamicActionKind>> getActs(
            GTSModelFactory mf, Theta theta, Set<Role> blocked, int c, int n) {
        return new LinkedHashSet<>();
    }

    @Override
    public Either<Exception, Triple<Theta, GTGType, Tree<String>>> step(
            Theta theta, SAction<DynamicActionKind> a, int c, int n) {
        return Either.left(newStuck(c, n, theta, this, (GTSAction) a));
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
        return GTUtil.umodSetOf();
    }

    @Override
    public Set<Op> getCommittingLeft(Role obs, Set<Role> com) {
        return GTUtil.umodSetOf();
    }

    @Override
    public Set<Op> getCommittingRight(Role obs, Set<Role> com) {
        return GTUtil.umodSetOf();
    }

    @Override
    public Pair<Set<Op>, Map<Integer, Pair<Set<Op>, Set<Op>>>> getLabels() {
        return Pair.of(GTUtil.setOf(), GTUtil.mapOf());
    }

    /* Aux */

    @Override
    public GTGType subs(Map<RecVar, GTGType> subs) {
        return this;
    }

    @Override
    public GTGEnd unfoldAllOnce() {
        return this;
    }

    @Override
    public Set<Role> getRoles() {
        return GTUtil.setOf();
    }

    @Override
    public Set<Integer> getTimeoutIds() {
        return Collections.emptySet();
    }

    @Override
    public Set<Op> getOps() {
        return Collections.emptySet();
    }

    @Override
    public Set<RecVar> getRecDecls() { return Collections.emptySet(); }

    @Override
    public String toString() {
        return "end";
    }

    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTGType.END_HASH;
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) { return true; }
        if (obj == null || !(obj instanceof GTGEnd)) { return false; }
        GTGEnd them = (GTGEnd) obj;
        return them.canEquals(this);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTGEnd;
    }
}
