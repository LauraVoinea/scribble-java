package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.GTSessType;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.util.Either;
import org.scribble.ext.gt.util.Tree;
import org.scribble.ext.gt.util.Triple;
import org.scribble.util.Pair;

import java.util.*;


public interface GTGTypeOps extends GTSessType {

    Optional<Pair<? extends GTLType, Sigma>> projectTop(Set<Role> topPeers, Role r);

    Optional<Theta> projectTheta(Set<Integer> cs, Role r);  // TODO refactor (cf. Theta.project)


    /* ... */

    Set<Role> getLiveRoles();

    // TODO refactor subs is singleton
    GTGType subs(RecVar v, GTGRecursion subs);

    Set<Integer> getTimeoutIds();  // c's


    /* ... GTSessType */

    // !!! cannot do once-unfold as-you-go (i.e., just subs), rec needs to do the subs then unfold after
    @Override
    GTGType unfoldAllImmediateRecs();  // unfold all rec prefixes -- non rec is idemp


    /* ... */
}
