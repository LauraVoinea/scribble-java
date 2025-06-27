package org.scribble.ext.gt.core.model;

import org.scribble.ast.global.GProtoDecl;
import org.scribble.core.type.name.Role;
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
        this.roles = Set.copyOf(roles);
        this.tids = Set.copyOf(tids);
        this.theta = theta;
        this.global = global;
        this.local = local;
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

    // CHECKME roles.equals(this.roles) ?
    // TODO move to GTGType
    public static Either<Exception, GTLSystem> projectTopLevel(
            Set<Role> roles, GTGType global, Set<Integer> cs) {
        Map<Role, GTLConfig> locals = new HashMap<>();
        for (Role r : roles) {
            Set<Role> peers = GTUtil.copyOf(roles);
            peers.remove(r);
            Optional<Pair<? extends GTLType, Sigma>> opt = global.projectTop(peers, r);
            if (!opt.isPresent()) {
                return Either.left(new Exception("Couldn't project onto " + r + ": " + global));
            }
            Pair<? extends GTLType, Sigma> p = opt.get();

            Optional<Theta> opt_theta = global.projectTheta(cs, r);
            if (!opt_theta.isPresent()) {
                return Either.left(new Exception("Couldn't project THETA for " + r + ": " + global));
            }

            locals.put(r, new GTLConfig(r, p.left, p.right, opt_theta.get(), GTUtil.mapOf()));
        }
        return Either.right(new GTLSystem(locals));
    }

    public static Set<Role> getRoles(GProtoDecl g) {
        // Could also just define GTGType.getRoles, cf. getTimeoutIds
        return g.getRoles().stream().collect(Collectors.toSet());
    }


}
