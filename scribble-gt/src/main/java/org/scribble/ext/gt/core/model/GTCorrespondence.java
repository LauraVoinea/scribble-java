package org.scribble.ext.gt.core.model;

import org.scribble.ast.global.GProtoDecl;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.GTLConfig;
import org.scribble.ext.gt.core.model.local.GTLSystem;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.global.GTGType;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

public class GTCorrespondence {

    public final Set<Role> roles;  // cf. Correspondence.getRoles
    public final Set<Integer> tids;

    public final Theta theta;
    public final GTGType global;

    public final GTLSystem local;

    // For initial starting global
    public GTCorrespondence(Set<Role> roles, GTGType global) {
        this(roles, global.getTimeoutIds(), new Theta(global.getTimeoutIds()), global, projectTopLevel(roles, global));
    }

    // In general, roles/tids (for original starting protocol) is superset of those in global
    public GTCorrespondence(Set<Role> roles, Set<Integer> tids, Theta theta, GTGType global, GTLSystem local) {
        this.roles = Collections.unmodifiableSet(new HashSet<>(roles));
        this.tids = Collections.unmodifiableSet(new HashSet<>(tids));
        this.theta = theta;
        this.global = global;
        this.local = local;
    }

    /* ... */

    // Checks projection correspondence between global and local
    public void check(String indent) {

        if (!this.roles.equals(this.local.configs.keySet())) {
            throw new RuntimeException("Roles mismatch: roles=" + this.roles + ", locals=" + this.local.configs.keySet());
        }

        if (!this.global.isGood()) {
            throw new RuntimeException("Not good: " + this.global);
        }
        if (!this.global.isCoherent()) {
            throw new RuntimeException("Not coherent: " + this.global);
        }

        GTLSystem projected = projectTopLevel(this.roles, this.global);
        for (Role r : this.roles) {
            GTLConfig p = projected.configs.get(r);
            System.out.println(indent + "Projected onto " + r + ": " + p);
            GTLConfig q = this.local.configs.get(r);

            //*
            //if (!p.equals(q)) {  // XXXXXX
            if (!p.isSubtype(q)) {
                throw new RuntimeException("Local config mismatch for " + r + ":\n\tprojected=" + p + "\n\tlocal=    " + q);
            }
            //*/

        }
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

    public static GTLSystem projectTopLevel(Set<Role> roles, GTGType global) {
        Theta theta = new Theta(global.getTimeoutIds());
        Map<Role, GTLConfig> locals = new HashMap<>();
        for (Role r : roles) {
            Optional<Pair<? extends GTLType, Sigma>> opt = global.projectTopLevel(roles, r);
            if (!opt.isPresent()) {
                throw new RuntimeException("Couldn't project onto " + r + ": " + global);
            }
            Pair<? extends GTLType, Sigma> p = opt.get();
            /*if (!p.right.equals(new Sigma(roles))) {
                throw new RuntimeException("Shouldn't get here: " + p);
            }*/
            locals.put(r, new GTLConfig(r, p.left, p.right, theta));
            //System.out.println("Project onto " + r + ": " + p.left);
        }
        return new GTLSystem(locals);
    }

    public static Set<Role> getRoles(GProtoDecl g) {
        // Could also just define GTGType.getRoles, cf. getTimeoutIds
        return g.getRoles().stream().collect(Collectors.toSet());
    }


}
