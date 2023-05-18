package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.global.action.GTSAction;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.GTSType;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.util.*;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

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

    default Optional<Pair<? extends GTLType, Sigma>> projectTop(Set<Role> rs, Role r) {
        return project(rs, r, GTLType.c_TOP, GTLType.n_INIT);
    }

    //Optional<Pair<? extends GTLType, Sigma>> project(Set<Role> rs, Role r);

    // rs for sigma_0
    Optional<Pair<? extends GTLType, Sigma>> project(Set<Role> rs, Role r, int c, int n);

    /*// cs for theta_0
    default Optional<Theta> projectThetaTop(Set<Integer> cs, Role r) {
        Optional<Theta> theta = projectTheta(cs, r);
        return theta.map(x -> {
            Map<Integer, Integer> tmp = GTUtil.copyOf(x.map);
            return new Theta(tmp.entrySet().stream().collect(Collectors.toMap(
                    Map.Entry::getKey,
                    y -> y.getValue() + 1
            )));
        });
    }*/

    Optional<Theta> projectTheta(Set<Integer> cs, Role r);  // TODO refactor

    /* ... */

    //HERE HERE make weak getActs/step for G/L -- make subtyping for MC (just structural?)

    // !!! c, n not _necessary_ for G reduction -- but needed(?) for fidelity
    default LinkedHashSet<SAction<DynamicActionKind>> getActsTop(
            GTSModelFactory mf, Theta theta) {
        return getActs(mf, theta, Collections.emptySet(), GTLType.c_TOP, GTLType.n_INIT);  // !!! from L type (could refactor)
    }

    // TODO GTSAction
    LinkedHashSet<SAction<DynamicActionKind>> getActs(
            GTSModelFactory mf, Theta theta, Set<Role> blocked, int c, int n);

    default Either<Exception, Triple<Theta, GTGType, Tree<String>>> stepTop(
            Theta theta, SAction<DynamicActionKind> a) {
        return step(theta, a, GTLType.c_TOP, GTLType.n_INIT);
    }

    // TODO GTSAction
    // a is deterministic (including "nested" steps)
    // c, n for action labels -- cf. projection (can derive c, n from MC syntax)
    Either<Exception, Triple<Theta, GTGType, Tree<String>>> step(
            Theta theta, SAction<DynamicActionKind> a, int c, int n);

    // FIXME: add c, n
    default Exception newStuck(int c, int n, Theta theta, GTGType t, GTSAction a) {
        return new Exception("Stuck: " + c + ", " + n + " " + ConsoleColors.VDASH + " "
                + theta + ", " + t + " --" + a + "-->");
    }

    // FIXME: add c, n
    default String toStepJudgeString(
            String tag, int c, int n, Theta theta_l, GTGType left, GTSAction a,
            Theta theta_r, GTGType right) {
        return tag + "  " + c + ", " + n + " " + ConsoleColors.VDASH + " "
                + theta_l + ", " + left + " --" + a + "--> " + theta_r + ", " + right;
    }

    /* ... */

    // \nu actions silent
    default LinkedHashSet<SAction<DynamicActionKind>> getWeakActsTop(
            GTSModelFactory mf, Theta theta) {
        return getWeakActs(mf, theta, Collections.emptySet(), GTLType.c_TOP, GTLType.n_INIT);  // !!! from L type (could refactor)
    }

    LinkedHashSet<SAction<DynamicActionKind>> getWeakActs(
            GTSModelFactory mf, Theta theta, Set<Role> blocked, int c, int n);

    default Either<Exception, Triple<Theta, GTGType, Tree<String>>> weakStepTop(
            Theta theta, SAction<DynamicActionKind> a) {
        return weakStep(theta, a, GTLType.c_TOP, GTLType.n_INIT);
    }

    // TODO GTSAction
    // a is deterministic (including "nested" steps)
    // c, n for action labels -- cf. projection (can derive c, n from MC syntax)
    Either<Exception, Triple<Theta, GTGType, Tree<String>>> weakStep(
            Theta theta, SAction<DynamicActionKind> a, int c, int n);

    /* ... */

    // Returns messages that when received on LHS mean role is committed to LHS, cf. [LRecv]
    default Set<Op> getCommittingTop() {
        return getCommittingTop(GTUtil.setOf());
    }

    Set<Op> getCommittingTop(Set<Role> com);
    //{ return GTUtil.mapOf(); }

    // com does NOT contain obs by default
    Set<Op> getCommittingLeft(Role obs, Set<Role> com);
    //{ return GTUtil.mapOf(); }

    // com does NOT contain obs by default
    Set<Op> getCommittingRight(Role obs, Set<Role> com);
    //{ return GTUtil.mapOf(); }

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
