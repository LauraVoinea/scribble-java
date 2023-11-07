package org.scribble.ext.gt.core.model;

import org.scribble.ast.global.GProtoDecl;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.GTLConfig;
import org.scribble.ext.gt.core.model.local.GTLSystem;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.global.GTGType;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.util.Either;
import org.scribble.ext.gt.util.GTUtil;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

public class GTCorrespondence {

    public final Set<Role> roles;  // initial top-level roles -- cf. dynamic Correspondence.getRoles
    public final Set<Integer> tids;  // "cs" -- timeoutids

    public final Theta theta;
    public final GTGType global;

    public final GTLSystem local;

    // For initial starting global
    public GTCorrespondence(Set<Role> roles, GTGType global) {
        this(roles, global.getTimeoutIds(), global);
    }

    // tids = cs
    protected GTCorrespondence(Set<Role> roles, Set<Integer> tids, GTGType global) {
        this(roles, tids, new Theta(tids), global, projectTopLevelWrap(roles, global, tids));
    }

    // tids = cs
    // In general, roles/tids (for original starting protocol) is superset of those in global
    public GTCorrespondence(Set<Role> roles, Set<Integer> tids, Theta theta, GTGType global, GTLSystem local) {
        this.roles = Collections.unmodifiableSet(new HashSet<>(roles));
        this.tids = Collections.unmodifiableSet(new HashSet<>(tids));
        this.theta = theta;
        this.global = global;
        this.local = local;
    }


    /* ... */

    /*public Optional<Exception> checkRuntimeProperties(GTSModelFactory mf, String indent) {
        return checkRuntimeProperties(mf, indent, true, true, true, true, true, true);
    }*/

    public Optional<Exception> checkRuntimeProperties(
            GTSModelFactory mf, String indent, Set<Integer> tids,
            boolean proj,  // Deprecated
            boolean cp, boolean ui, boolean co, boolean sd, boolean ct, boolean ac) {

        // local ?

        /*if (proj) {  // !!! cf. checkProjectionCorrespondence
            Either<Exception, GTLSystem> p = projectTopLevel(roles, global, tids);
            if (p.isLeft()) {
                return Optional.of(p.getLeft());
            }
        }*/

        // global

        if (cp && !this.global.isChoicePartip()) {
            return Optional.of(new Exception("Not choice-participating: " + this.global));
        }

        if (ui && !this.global.isUniqueInstan()) {
            return Optional.of(new Exception("Not unique instantiating: " + this.global));
        }

        if (co && !this.global.isCoherent()) {
            return Optional.of(new Exception("Not coherent: " + this.global));
        }

        if (sd && !this.global.isSingleDecision(this.roles, this.theta)) {
            return Optional.of(new Exception("Not single-decision: " + this.global));
        }

        if (ct && !this.global.isClearTermination()) {
            return Optional.of(new Exception("Not clear-termination: " + this.global));
        }

        if (ac && !this.global.isAwareCorollary(mf, this.roles, this.theta)) {  // FIXME deprecate mf
            return Optional.of(new Exception("Not aware corollary: " + this.global));
        }

        // OK
        return Optional.empty();
    }


    /* ... */

    // Checks projection correspondence between global and local
    public Optional<Exception> checkProjectionCorrespondence(
            boolean debug, GTSModelFactory mf, String indent) {

        if (!this.roles.equals(this.local.configs.keySet())) {
            throw new RuntimeException("Roles mismatch: roles=" + this.roles + ", locals=" + this.local.configs.keySet());
        }

        /*if (!this.global.isGood()) {
            throw new RuntimeException("Not good: " + this.global);
        }*/
        /*if (!this.global.isAwareCorollary(mf, this.theta)) {  // cf. checkRuntimeProperties
            return Optional.of(new Exception("Not run-time aware: " + this.global));
        }
        if (!this.global.isCoherent()) {
            return Optional.of(new Exception("Not coherent: " + this.global));
        }*/

        Either<Exception, GTLSystem> e = projectTopLevel(this.roles, this.global, this.tids);
        if (e.isLeft()) {
            return Optional.of(e.getLeft());
        }
        GTLSystem projected = e.getRight();
        for (Role r : this.roles) {
            GTLConfig p = projected.configs.get(r);
            if (debug) {
                System.out.println(indent + "Projected onto " + r + ": " + p);  // FIXME UI output
            }
            GTLConfig q = this.local.configs.get(r);

            /*if (debug) {
                System.out.println(indent + "Checking Local subtype of Projection: " + q + " <: " + p);  // FIXME UI output
            }*/
            //*
            //if (!p.equals(q)) {  // XXXXXX -- N.B. equality including GC env etc.
            if (!q.isSubtype(p)) {
                return Optional.of(new Exception("Local config mismatch for " + r + ":\n\tprojected=" + p + "\n\tlocal=    " + q));
            }
            //*/

            // !!! XXX FIXME check Sigma, Theta and Discard

        }

        return Optional.empty();
    }

    /* ... */

    @Override
    public String toString() {
        return toString("");
    }

    public String toString(String indent) {
        return indent + "Global = (" + this.theta + ", " + this.global + ")\n"
                + indent + "Local  = " + this.local;
    }

    /* ... */

    // cs for projectTheta theta_0
    public static GTLSystem projectTopLevelWrap(Set<Role> roles, GTGType global, Set<Integer> cs) {
        Either<Exception, GTLSystem> e = projectTopLevel(roles, global, cs);
        if (e.isLeft()) {
            throw new RuntimeException(e.getLeft().getCause());
        }
        return e.getRight();
    }

    // CHECKME roles.equals(this.roles) ? -- deprecate roles param below?
    // TODO move to GTGType
    public static Either<Exception, GTLSystem> projectTopLevel(
            Set<Role> roles, GTGType global, Set<Integer> cs) {
        //Theta theta = new Theta(global.getTimeoutIds());
        Map<Role, GTLConfig> locals = new HashMap<>();
        for (Role r : roles) {
            Set<Role> peers = GTUtil.copyOf(roles);
            peers.remove(r);
            Optional<Pair<? extends GTLType, Sigma>> opt = global.projectTop(peers, r);
            if (!opt.isPresent()) {
                //throw new RuntimeException("Couldn't project onto " + r + ": " + global);
                return Either.left(new Exception("Couldn't project onto " + r + ": " + global));
            }
            Pair<? extends GTLType, Sigma> p = opt.get();
            /*if (!p.right.equals(new Sigma(roles))) {
                throw new RuntimeException("Shouldn't get here: " + p);
            }*/

            Optional<Theta> opt_theta = global.projectTheta(cs, r);
            if (!opt_theta.isPresent()) {
                //throw new RuntimeException("Couldn't project onto " + r + ": " + global);
                return Either.left(new Exception("Couldn't project onto " + r + ": " + global));
            }

            //locals.put(r, new GTLConfig(r, p.left, p.right, opt_theta.get()));
            locals.put(r, new GTLConfig(r, p.left, p.right, opt_theta.get(), GTUtil.mapOf()));
            //System.out.println("Project onto " + r + ": " + p.left);
        }
        return Either.right(new GTLSystem(locals));
    }

    public static Set<Role> getRoles(GProtoDecl g) {
        // Could also just define GTGType.getRoles, cf. getTimeoutIds
        return g.getRoles().stream().collect(Collectors.toSet());
    }


}
