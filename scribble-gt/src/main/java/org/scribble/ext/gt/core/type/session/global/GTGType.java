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
import org.scribble.ext.gt.core.type.session.GTSessType;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.util.*;
import org.scribble.util.Pair;

import java.util.*;


public interface GTGType extends GTSessType, GTGTypeOps {

    int GLOBAL_END_HASH = 1663;
    int GLOBAL_CHOICE_HASH = 1667;
    int GLOBAL_WIGGLY_HASH = 1669;
    int GLOBAL_MIXED_DEF_HASH = 1693;
    int GLOBAL_MIXED_ACTIVE_HASH = 1697;
    int GLOBAL_REC_HASH = 1699;
    int GLOBAL_RECVAR_HASH = 1709;


    /* ... static only */

    // Initial and well-set -- well-set => initial  // TODO refactor using choice-partic and timeout-partic/pattern
    @Override
    default boolean isInitialWellSet() { return isInitialWellSet(GTUtil.setOf()); }

    boolean isInitialWellSet(Set<Integer> cs);

    // TODO
    // - timeout-partic -- XXX balance
    // - timeout-pattern -- cf. isSinglePointed


    /* ... -- top-down, no global weak */

    /*// !!! c, n not _necessary_ for G reduction -- but needed(?) for fidelity
    default LinkedHashSet<SAction<DynamicActionKind>> getActsTop(
            GTSModelFactory mf, Theta theta) {
        return getActs(mf, theta, Collections.emptySet(), GTLType.c_TOP, GTLType.n_INIT);  // !!! from L type (could refactor)
    }*/

    default LinkedHashMap<SAction<DynamicActionKind>, Set<RecVar>> getActsTop(
            GTSModelFactory mf,  // TODO remove
            Theta theta) {
        return getActs(mf, theta, Collections.emptySet(), GTLType.c_TOP, GTLType.n_INIT);  // CHECKME LType, cf. GTGType.getWeakActsTop
    }

    // TODO GTSAction
    //LinkedHashSet<SAction<DynamicActionKind>> getActs(
    LinkedHashMap<SAction<DynamicActionKind>, Set<RecVar>> getActs(  // HERE HERE cf. GTLType,
                                                                     GTSModelFactory mf,  // TODO remove
                                                                     Theta theta, Set<Role> blocked, int c, int n);

    default Either<Exception, Triple<Theta, GTGType, Tree<String>>> stepTop(
            Theta theta, SAction<DynamicActionKind> a) {
        return step(theta, a, GTLType.c_TOP, GTLType.n_INIT);
    }

    // TODO GTSAction
    // a is deterministic (including "nested" steps)
    // c, n for action labels -- cf. projection (can derive c, n from MC syntax)
    Either<Exception, Triple<Theta, GTGType, Tree<String>>> step(
            Theta theta, SAction<DynamicActionKind> a, int c, int n);

    default Exception newStepStuck(int c, int n, Theta theta, GTGType t, GTSAction a) {
        return new Exception("Stuck: " + c + ", " + n + " " + ConsoleColors.VDASH + " "
                + theta + ", " + t + " --" + a + "-->");
    }

    default String toStepJudgeString(
            String tag, int c, int n, Theta theta_l, GTGType left, GTSAction a,
            Theta theta_r, GTGType right) {
        return tag + "  " + c + ", " + n + " " + ConsoleColors.VDASH + " "
                + theta_l + ", " + left + " --" + a + "--> " + theta_r + ", " + right;
    }


    /* ... preserved -- check */

    boolean isRuntimeChoicePartip();  // cf. "static" choice-partic in isInitialAndWellSet

    default boolean isUniqueInstan() { return isUniqueInstan(GTUtil.setOf()); }

    boolean isUniqueInstan(Set<Pair<Integer, Integer>> seen);


    /* ... preserved -- check */

    // boolean isBalanced();  // TODO

    // CHECKME: Theta not used for "static" version?
    // ...doesn't check "initial"
    boolean isSingleDecision(Set<Role> topAll, Theta theta);  // cf. topPeers in project

    // ..."top-level" left-committing check -- cf. find all mixed-choice within G
    // !!! CHECKME "approx" of awareness clear-termination -- cf. LHS weak-deps to obs
    boolean isClearTermination();


    /* ... preserved -- check */

    // LR-initiation
    boolean isAwareCorollary(GTSModelFactory mf, Set<Role> topAll, Theta theta);  // FIXME refactor mf out of params


    /* ... preserved -- check */

    boolean isCoherent();  // TODO well-set => coherent -- coherent + full participation should be preserved -- TODO rename?


    /* ... -- fidelity */

    // \nu actions silent
    default LinkedHashSet<SAction<DynamicActionKind>> getWeakActsTop(
            GTSModelFactory mf,  // TODO remove
            Theta theta) {
        return getWeakActs(mf, theta, Collections.emptySet(), GTLType.c_TOP, GTLType.n_INIT);  // !!! from L type (could refactor)
    }

    LinkedHashSet<SAction<DynamicActionKind>> getWeakActs(
            GTSModelFactory mf,  // TODO remove
            Theta theta, Set<Role> blocked, int c, int n);

    default Either<Exception, Triple<Theta, GTGType, Tree<String>>> weakStepTop(
            Theta theta, SAction<DynamicActionKind> a) {
        return weakStep(theta, a, GTLType.c_TOP, GTLType.n_INIT);
    }

    // TODO GTSAction
    // a is deterministic (including "nested" steps) -- weak is excluding \nu
    // c, n for action labels -- cf. projection (can derive c, n from MC syntax)
    Either<Exception, Triple<Theta, GTGType, Tree<String>>> weakStep(
            Theta theta, SAction<DynamicActionKind> a, int c, int n);


    /* ... */

    default Optional<Pair<? extends GTLType, Sigma>> projectTop(Set<Role> topPeers, Role r) {
        return project(topPeers, r, GTLType.c_TOP, GTLType.n_INIT);
    }

    // topPeers for sigma_0
    Optional<Pair<? extends GTLType, Sigma>> project(Set<Role> topPeers, Role r, int c, int n);

    Optional<Theta> projectTheta(Set<Integer> cs, Role r);  // TODO refactor (cf. Theta.project)


    /* ... */

    // N.B. indiff is mixed-choice/active only (not all globals)
    Map<Role, Set<Role>> getStrongDeps();

    // Returns messages that when received on LHS mean role is committed to LHS, cf. [LRecv]
    default Set<Op> getCommittingTop() {
        return getCommittingTop(GTUtil.setOf());
    }

    Set<Op> getCommittingTop(Set<Role> com);

    // com does NOT contain obs by default
    Set<Op> getCommittingLeft(Role obs, Set<Role> com);

    // com does NOT contain obs by default
    Set<Op> getCommittingRight(Role obs, Set<Role> com);


    /* ... */

    // TODO refactor subs is singleton
    @Override
    GTGType subs(RecVar v, GTGRecursion subs);

    // !!! cannot do once-unfold as-you-go (i.e., just subs), rec needs to do the subs then unfold after
    @Override
    GTGType unfoldAllOnce();

    //GTGType unfoldContext(Map<RecVar, GTGType> c);

    // cf. get(Weak)Acts, "bypass" Theta, c, n
    default Set<Role> getReady() { return getReadyAux(Collections.emptySet()); }

    Set<Role> getReadyAux(Set<Role> blocked);

    Set<Role> getRoles();

    Set<Integer> getTimeoutIds();  // c's

    Set<Op> getOps();

    Set<RecVar> getRecDecls();

    // left = "current", right = c -> (left, right) -- the "immediate" discardable labels of a timeout c -- not nested ones, reduction would use the nested c' tag
    // ingore non-mc or mergable in c, never discarded
    Pair<Set<Op>, Map<Integer, Pair<Set<Op>, Set<Op>>>> getLabels();


























    /* deprecated */

    @Deprecated
    boolean isSinglePointed();  // TODO -> well-set?  // Initial WF -- !!! includes mixed-choice distinct labels check -- currently "globally" distinct using getOps

    @Deprecated
    boolean isGood();  // TODO -> full participation?  // !!! includes wiggly op annot check

    // well-set -- init WF
    // coherence -- run-time invariant (lemma 3)

    // ...G aware Theta -- all t in G aware Theta

    // lemma 4: "aware" + coherent => progress
    // theorem 1: well-set + choice-participation => progress

    // "awareness properties" -- run-time invariant (lemma 2)

    /* ... */

    @Deprecated
    boolean isInitial();


    /* ... */

    @Deprecated
    boolean isLeftCommitting(Set<Role> com, Set<Role> rem);  // ...except for GTMixedChoice

    // ...left-committing check under the context of a specific mixed-choice instance
    boolean isLeftCommittingAux(Role obs, Set<Role> com, Set<Role> rem);


}
