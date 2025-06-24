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






    /* ... preserved -- check */

    Optional<Pair<? extends GTLType, Sigma>> projectTop(Set<Role> topPeers, Role r);

    Optional<Theta> projectTheta(Set<Integer> cs, Role r);  // TODO refactor (cf. Theta.project)


    /* ... */

    // Returns messages that when received on LHS mean role is committed to LHS, cf. [LRecv]
    Map<Role, Set<Op>> getCommittingTop();


    /* ... */

    Set<Role> getLiveRoles();

    // cf. get(Weak)Acts, "bypass" Theta, c, n
    Set<Role> getReady();


    /* ... GTSessType */

    // !!! cannot do once-unfold as-you-go (i.e., just subs), rec needs to do the subs then unfold after
    @Override
    GTGType unfoldAllImmediateRecs();  // unfold all rec prefixes -- non rec is idemp


    /* ... */

    // TODO refactor subs is singleton
    GTGType subs(RecVar v, GTGRecursion subs);

    //GTGType unfoldContext(Map<RecVar, GTGType> c);

    Set<Integer> getTimeoutIds();  // c's

    Set<Op> getOps();

    // left = "current", right = c -> (left, right) -- the "immediate" discardable labels of a timeout c -- not nested ones, reduction would use the nested c' tag
    // ingore non-mc or mergable in c, never discarded
    Pair<Set<Op>, Map<Integer, Pair<Set<Op>, Set<Op>>>> getLabels();

    Set<RecVar> getRecDecls();
}
