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

    // Initial and well-set  // TODO refactor using choice-partic and timeout-partic/pattern
    default boolean isInitialWellSet() { return isInitialWellSet(GTUtil.setOf()); }

    boolean isInitialWellSet(Set<Integer> cs);

    Map<Role, Set<Role>> getStrongDeps();


    /* ... */

    boolean isChoicePartip();  // "dynamic" choice-partipication, cf. initial well-set

    default boolean isUniqueInstan() { return isUniqueInstan(GTUtil.setOf()); }

    boolean isUniqueInstan(Set<Pair<Integer, Integer>> seen);

    boolean isCoherent();  // TODO well-set => coherent -- coherent + full participation should be preserved -- TODO rename?

    // ...well-nested


    /* ... */

    //HERE HERE HERE separate run-time aware from aware corollaries -- refactor foo loop to enable testing different run-time properties

    // CHECKME: Theta not used for "static" version?
    // !!! currently just single-decision -- clear-termination approx by isLeftCommitting(Top)
    // ...doesn't check "initial"
    boolean isSingleDecision(Theta theta);

    // boolean isBalanced();  // TODO awareness

    // ..."top-level" left-committing check -- cf. find all mixed-choice within G
    // !!! CHECKME "approx" of awareness clear-termination -- cf. LHS weak-deps to obs
    boolean isClearTermination();

    //default boolean isLeftCommitting() { return isLeftCommitting(GTUtil.setOf(), getRoles()); }

    @Deprecated
    boolean isLeftCommitting(Set<Role> com, Set<Role> rem);  // ...except for GTMixedChoice

    // ...left-committing check under the context of a specific mixed-choice instance
    boolean isLeftCommittingAux(Role obs, Set<Role> com, Set<Role> rem);

    boolean isAwareCorollary(GTSModelFactory mf, Theta theta);  // FIXME refactor mf out of params


    /* ... */

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

    // left = "current", right = c -> (left, right) -- the "immediate" discardable labels of a timeout c -- not nested ones, reduction would use the nested c' tag
    // ingore non-mc or mergable in c, never discarded
    Pair<Set<Op>, Map<Integer, Pair<Set<Op>, Set<Op>>>> getLabels();

    /* ... */

    // TODO refactor subs is singleton
    GTGType subs(Map<RecVar, GTGType> subs);

    // !!! cannot do once-unfold as-you-go (i.e., just subs), rec needs to do the subs then unfold after
    @Override
    GTGType unfoldAllOnce();

    //GTGType unfoldContext(Map<RecVar, GTGType> c);

    Set<Role> getRoles();

    Set<Integer> getTimeoutIds();  // c's

    Set<Op> getOps();
}
