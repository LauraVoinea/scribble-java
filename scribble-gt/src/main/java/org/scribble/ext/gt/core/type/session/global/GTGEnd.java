package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.core.type.session.local.GTLTypeFactory;
import org.scribble.ext.gt.util.GTUtil;
import org.scribble.util.Pair;

import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

// !!! No "fid"
public class GTGEnd implements GTGType {

    public static final GTGEnd END = new GTGEnd();

    protected GTGEnd() { }


    @Override
    public Optional<Exception> isInitialAndpq() {
        return Optional.empty();
    }

    @Override
    public Set<Role> getLiveRoles() {
        return GTUtil.setOf();
    }

    @Override
    public GTGType unfoldAllOnceAux(Set<RecVar> recvars) {
        return this;
    }

    // !!! assumes unfolded all once
    @Override
    public Set<Op> getChoiceLabelsUpTo(int c) {
        return Collections.emptySet();
    }

    @Override
    public Optional<Exception> checkWellFormed() {
        return Optional.empty();
    }

    @Override
    public Optional<Exception> checkedFailedAnnotsAux(Set<Role> failed) {
        return Optional.empty();
    }

    @Override
    public Map<Role, Set<Op>> getExplicitCommittingAux(int c, Set<Role> com) {
        return Collections.emptyMap();
    }

    @Override
    public Map<Role, Set<Op>> getCommittingAuxNew(int c, Set<Role> com) {
        return Collections.emptyMap();
    }

    @Override
    public Set<Integer> getTimeoutIds() {
        return Collections.emptySet();
    }

    @Override
    public Map<Role, Set<Role>> getStrictSyntacticDeps() {
        return getSyntacticDeps();
    }

    protected Map<Role, Set<Role>> getSyntacticDeps() {
        return Collections.emptyMap();
    }

    @Override
    public Map<Role, Set<Role>> getEventualSyntacticDeps() {
        return getSyntacticDeps();
    }

    @Override
    public boolean isDiverging() {
        return false;
    }

    @Override
    public Set<RecVar> getFreeRecVars() {
        return Collections.emptySet();
    }

    @Override
    public Optional<Exception> isSyntacticAware() {
        return Optional.empty();
    }

    @Override
    public Optional<Exception> isBalanced() {
        return Optional.empty();
    }

    @Override
    public String format(String pref) {
        return pref + "end";
    }





    // OLD


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
    public Map<Role, Set<Op>> getCommittingAux(int c, Set<Role> com) {
        return GTUtil.umodMapOf();
    }



    /* Aux */

    @Override
    public GTGType subs(RecVar v, GTGRecursion subs) {
        return this;
    }

    @Override
    public String toString() {
        return "end";
    }

    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTGType.GLOBAL_END_HASH;
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













    /* ... */

    @Override
    public boolean isRuntimeChoicePartip() {
        return true;
    }

    @Override
    public boolean isUniqueInstan(Set<Pair<Integer, Integer>> seen) {
        return true;
    }

    @Override
    public boolean isAwareCorollary(GTSModelFactory mf, Set<Role> topAll, Theta theta) {
        return true;
    }

    @Override
    public boolean isCoherent() {
        return true;
    }

}
