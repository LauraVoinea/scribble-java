package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.GTSessType;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.util.Pair;

import java.util.Optional;
import java.util.Set;


public interface GTGTypeOps extends GTSessType {

    Optional<Pair<? extends GTLType, Sigma>> projectTop(Set<Role> topPeers, Role r);

    Optional<Theta> projectTheta(Set<Integer> cs, Role r);  // TODO refactor (cf. Theta.project)


    /* ... */

    Set<Role> getLiveRoles();

    GTGType subs(RecVar v, GTGRecursion subs);

    Set<Integer> getTimeoutIds();  // c's


}
