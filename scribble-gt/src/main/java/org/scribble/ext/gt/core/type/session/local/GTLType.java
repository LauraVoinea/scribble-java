package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.GTEModelFactory;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.model.local.action.GTEAction;
import org.scribble.ext.gt.core.type.session.GTSType;
import org.scribble.ext.gt.util.ConsoleColors;
import org.scribble.ext.gt.util.Either;
import org.scribble.ext.gt.util.Quad;
import org.scribble.ext.gt.util.Tree;

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
    default Either<Exception, Quad<GTLType, Sigma, Theta, Tree<String>>> stepTopLevel(
            Set<Op> com, Role self, EAction<DynamicActionKind> a, Sigma sigma, Theta theta) {
        return step(com, self, a, sigma, theta, GTLType.c_TOP, GTLType.n_INIT);
    }

    // TODO GTEAction
    // a is deterministic (including "nested" steps)
    Either<Exception, Quad<GTLType, Sigma, Theta, Tree<String>>> step(
            Set<Op> com, Role self, EAction<DynamicActionKind> a, Sigma sigma, Theta theta, int c, int n);

    default LinkedHashSet<EAction<DynamicActionKind>> getActsTopLevel(
            GTEModelFactory mf, Role self, Sigma sigma, Theta theta) {
        return getActs(mf, self, Collections.emptySet(), sigma, theta, GTLType.c_TOP, GTLType.n_INIT);
    }

    default Exception newStuck(int c, int n, Theta theta, GTLType t, GTEAction a) {
        return new Exception("Stuck: " + c + ", " + n + " " + ConsoleColors.VDASH + " "
                + theta + ", " + t + " --" + a + "-->");
    }

    default String toStepJudgeString(
            String tag, int c, int n, Theta theta_l, GTLType left, Sigma sigma_l, GTEAction a,
            Theta theta_r, GTLType right, Sigma sigma_r) {
        return tag + "  " + c + ", " + n + " " + ConsoleColors.VDASH + " "
                + theta_l + ", " + left + " --" + a + "--> " + theta_r + ", " + right;
    }

    // TODO remove blocked
    // TODO GTEAction
    LinkedHashSet<EAction<DynamicActionKind>> getActs(
            GTEModelFactory mf, Role self, Set<Role> blocked, Sigma sigma, Theta theta, int c, int n);

    /* ... */

    @Override
    default GTLType unfold() {
        return unfoldContext(Collections.emptyMap());
    }

    // Substitution inlined into this op -- probably better to separate unf/subs
    GTLType unfoldContext(Map<RecVar, GTLType> env);
}
