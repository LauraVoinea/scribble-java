package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.GTSType;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.util.Triple;
import org.scribble.util.Pair;

import java.util.*;

public interface GTGType extends GTSType { //<Global, GSeq>, GNode {

    int END_HASH = 1663;
    int CHOICE_HASH = 1667;
    int WIGGLY_HASH = 1669;
    int MIXED_CHOICE_HASH = 1693;
    int MIXED_CHOICE_ACTIVE_HASH = 1697;
    int REC_HASH = 1699;
    int RECVAR_HASH = 1709;

    /* ... */

    boolean isSinglePointed();  // TODO -> well-set?  // Initial WF -- !!! includes mixed-choice distinct labels check -- currently "globally" distinct using getOps

    boolean isCoherent();  // TODO well-set => coherent -- coherent + full participation should be preserved -- TODO rename?

    boolean isGood();  // TODO -> full participation?  // !!! includes wiggly op annot check

    default Optional<Pair<? extends GTLType, Sigma>> projectTopLevel(Set<Role> rs, Role r) {
        return project(rs, r, GTLType.c_TOP, GTLType.n_INIT);
    }

    //Optional<Pair<? extends GTLType, Sigma>> project(Set<Role> rs, Role r);

    Optional<Pair<? extends GTLType, Sigma>> project(Set<Role> rs, Role r, int c, int n);

    /* ... */

    default Optional<Triple<Theta, GTGType, String>> stepTopLevel(Theta theta, SAction<DynamicActionKind> a) {
        return step(theta, a, GTLType.c_TOP, GTLType.n_INIT);
    }

    // a is deterministic (including "nested" steps)
    Optional<Triple<Theta, GTGType, String>> step(Theta theta, SAction<DynamicActionKind> a, int c, int n);

    //HERE HERE fix SAction kinds and add c, n
    // !!! c, n not _necessary_ for G reduction -- but needed(?) for fidelity
    default LinkedHashSet<SAction<DynamicActionKind>> getActsTopLevel(GTSModelFactory mf, Theta theta) {
        return getActs(mf, theta, Collections.emptySet(), GTLType.c_TOP, GTLType.n_INIT);  // !!! from L type (could refactor)
    }

    LinkedHashSet<SAction<DynamicActionKind>> getActs(GTSModelFactory mf, Theta theta, Set<Role> blocked, int c, int n);

    /* ... */

    // CHECKME factor out subs?
    @Override
    default GTGType unfold() {
        return unfoldContext(Collections.emptyMap());
    }

    GTGType unfoldContext(Map<RecVar, GTGType> c);

    Set<Integer> getTimeoutIds();

    Set<Op> getOps();
}
