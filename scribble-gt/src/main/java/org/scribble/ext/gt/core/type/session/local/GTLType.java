package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.endpoint.EModelFactory;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.GTSType;
import org.scribble.util.Pair;

import java.util.*;

public interface GTLType extends GTSType { //<Global, GSeq>, GNode {

    int END_HASH = 9851;
    int BRANCH_HASH = 9857;
    int SELECT_HASH = 9859;
    int MIXED_CHOICE_HASH = 9871;
    int MIXED_CHOICE_ACTIVE_HASH = 9883;
    int REC_HASH = 9887;
    int RECVAR_HASH = 9901;

    int c_TOP = -1;
    int n_INIT = 1;

    /* ... */

    // this merge g  -- should be symmetric
    Optional<? extends GTLType> merge(GTLType t);
    //return this.equals(t) ? Optional.of(this) : Optional.empty();
    //return GTGInteraction.merge(Optional.of(this), Optional.of(t));

    /* ... */

    // FIXME: Sigma may be local or remote depending on action
    default Optional<Pair<GTLType, Sigma>> stepTopLevel(Role self, EAction<DynamicActionKind> a, Sigma sigma) {
        return step(self, a, sigma, GTLType.c_TOP, GTLType.n_INIT);
    }

    // GTESend, GTERecv
    // a is deterministic (including "nested" steps)
    Optional<Pair<GTLType, Sigma>> step(Role self, EAction<DynamicActionKind> a, Sigma sigma, int c, int n);

    default LinkedHashSet<EAction<DynamicActionKind>> getActs(EModelFactory mf, Role self, Sigma sigma, int c, int n) {
        return getActs(mf, self, Collections.emptySet(), sigma, c, n);
    }

    LinkedHashSet<EAction<DynamicActionKind>> getActs(EModelFactory mf, Role self, Set<Role> blocked, Sigma sigma, int c, int n);

    /* ... */

    @Override
    default GTLType unfold() {
        return unfoldContext(Collections.emptyMap());
    }

    // Substitution inlined into this op -- probably better to separate unf/subs
    GTLType unfoldContext(Map<RecVar, GTLType> env);
}
