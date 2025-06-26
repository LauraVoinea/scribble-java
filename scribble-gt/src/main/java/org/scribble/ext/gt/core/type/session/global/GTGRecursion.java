package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.local.GTLRecVar;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.core.type.session.local.GTLTypeFactory;
import org.scribble.ext.gt.util.ConsoleColors;
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
    public Optional<Exception> isInitialAndpq() {
        return this.body.isInitialAndpq();
    }

    @Override
    public Set<Role> getLiveRoles() {
        return this.body.getLiveRoles();
    }

    @Override
    public GTGRecursion subs(RecVar v, GTGRecursion subs) {
        if (this.var.equals(v)) {
            return this;
        }
        return new GTGRecursion(this.var, this.body.subs(v, subs));
    }

    @Override
    public GTGType unfoldAllOnceAux(Set<RecVar> recvars) {
        if (recvars.contains(this.var)) {
            return this;
        } else {
            Set<RecVar> tmp = new HashSet<>(recvars);
            tmp.add(this.var);
            return this.body.subs(this.var, this).unfoldAllOnceAux(tmp);
        }
    }

    // assumes unfolded all once
    @Override
    public Set<Op> getChoiceLabelsUpTo(int c) {
        return Collections.emptySet();
    }

    // assumes unfolded all once
    @Override
    public Optional<Exception> checkWellFormed() {
        return Optional.empty();
    }

    @Override
    public Optional<Exception> checkedFailedAnnotsAux(Set<Role> failed) {
        return this.body.checkedFailedAnnotsAux(failed);
    }

    @Override
    public Map<Role, Set<Op>> getExplicitCommittingAux(int c, Set<Role> com) {
        return this.body.getExplicitCommittingAux(c, com);
    }

    @Override
    public Map<Role, Set<Op>> getCommittingAuxNew(int c, Set<Role> com) {
        return this.body.getCommittingAuxNew(c, com);
    }

    @Override
    public Set<Integer> getTimeoutIds() {
        return this.body.getTimeoutIds();
    }

    @Override
    public Map<Role, Set<Role>> getStrictSyntacticDeps() {
        return this.body.getStrictSyntacticDeps();
    }

    @Override
    public Map<Role, Set<Role>> getEventualSyntacticDeps() {
        return this.body.getEventualSyntacticDeps();
    }

    @Override
    public boolean isDiverging() {
        return this.body.isDiverging();
    }

    @Override
    public Set<RecVar> getFreeRecVars() {
        Set<RecVar> res = new HashSet<>(this.body.getFreeRecVars());
        res.remove(this.var);
        return res;
    }

    @Override
    public Optional<Exception> isSyntacticAware() {
        return this.body.isSyntacticAware();
    }

    @Override
    public Optional<Exception> isBalanced() {
        return this.body.isBalanced();
    }

    @Override
    public String format(String pref) {
        return pref + "mu " + this.var + "." +
                "\n" + this.body.format(pref + "    ");
    }


    /* ... */

    @Override
    public Optional<Pair<? extends GTLType, Sigma>> project(Set<Role> topPeers, Role r, int c, int n) {
        GTLTypeFactory lf = GTLTypeFactory.FACTORY;
        return this.body.project(topPeers, r, c, n).map(x ->
                x.left instanceof GTLRecVar cast && cast.var.equals(this.var)
                ? Pair.of(lf.end(), new Sigma(topPeers))
                : Pair.of(lf.recursion(this.var, x.left), x.right)
        );
    }

    @Override
    public Optional<Theta> projectTheta(Set<Integer> cs, Role r) {
        return Optional.of(new Theta(cs));
    }


    /* ... */

    @Override
    public Map<Role, Set<Op>> getCommittingAux(int c, Set<Role> com) {
        return this.body.getCommittingAux(c, com);
    }

    @Override
    public String toString() {
        return ConsoleColors.toRecString("mu " + this.var + "." + this.body);
    }


    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTGType.GLOBAL_REC_HASH;
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
