package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.model.endpoint.EModelFactory;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.type.session.GTSType;

import java.util.*;

public interface GTLType extends GTSType { //<Global, GSeq>, GNode {

    int END_HASH = 9851;
    int BRANCH_HASH = 9857;
    int SELECT_HASH = 9859;
    int MIXED_CHOICE_HASH = 9871;
    int MIXED_CHOICE_ACTIVE_HASH = 9883;
    int REC_HASH = 9887;
    int RECVAR_HASH = 9901;

    @Override
    default GTLType unfold() {
        return unfoldContext(Collections.emptyMap());
    }

    GTLType unfoldContext(Map<RecVar, GTLType> env);

    // this merge g  -- should be symmetric
    Optional<? extends GTLType> merge(GTLType t);
    //return this.equals(t) ? Optional.of(this) : Optional.empty();
    //return GTGInteraction.merge(Optional.of(this), Optional.of(t));

    // a is deterministic (including "nested" steps)
    Optional<GTLType> step(EAction a);

    default LinkedHashSet<EAction> getActs(EModelFactory mf) {
        return getActs(mf, Collections.emptySet());
    }

    LinkedHashSet<EAction> getActs(EModelFactory mf, Set<Role> blocked);
}
