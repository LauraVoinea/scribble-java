package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.lang.global.GNode;
import org.scribble.core.model.global.SModelFactory;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.kind.Global;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.global.GSeq;
import org.scribble.ext.gt.core.type.session.GTSType;
import org.scribble.ext.gt.core.type.session.local.GTLType;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Optional;
import java.util.Set;

public interface GTGType extends GTSType { //<Global, GSeq>, GNode {

    int END_HASH = 1663;
    int CHOICE_HASH = 1667;
    int WIGGLY_HASH = 1669;
    int MIXED_CHOICE_HASH = 1693;

    Optional<? extends GTLType> project(Role r);

    boolean isSinglePointed();  // Initial WF -- !!! includes mixed-choice distinct labels check -- currently "globally" distinct using getOps
    boolean isGood();  // !!! includes wiggly op annot check

    boolean isCoherent();  // TODO rename
    //boolean isLemma2();
    //boolean isLemma3();

    // a is deterministic (including "nested" steps)
    Optional<GTGType> step(SAction a);

    default LinkedHashSet<SAction> getActs(SModelFactory mf) {
        return getActs(mf, Collections.emptySet());
    }

    LinkedHashSet<SAction> getActs(SModelFactory mf, Set<Role> blocked);
    Set<Op> getOps();
}
