package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.model.endpoint.EModelFactory;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.type.session.GTSType;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Optional;
import java.util.Set;

public interface GTLType extends GTSType { //<Global, GSeq>, GNode {

    int END_HASH = 9851;
    int BRANCH_HASH = 9857;
    int SELECT_HASH = 9859;
    int MIXED_CHOICE_HASH = 9871;

    // this merge g  -- should be symmetric
    default Optional<GTLType> merge(GTLType t) {
        return this.equals(t) ? Optional.of(this) : Optional.empty();
    }

    // a is deterministic (including "nested" steps)
    Optional<GTLType> step(EAction a);

    default LinkedHashSet<EAction> getActs(EModelFactory mf) {
        return getActs(mf, Collections.emptySet());
    }

    LinkedHashSet<EAction> getActs(EModelFactory mf, Set<Role> blocked);
}
