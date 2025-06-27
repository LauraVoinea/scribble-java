package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.GTSessType;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;


public interface GTGType extends GTSessType, GTGTypeOps {

    int GLOBAL_END_HASH = 1663;
    int GLOBAL_CHOICE_HASH = 1667;
    int GLOBAL_MIXED_DEF_HASH = 1693;
    int GLOBAL_REC_HASH = 1699;
    int GLOBAL_RECVAR_HASH = 1709;

    int c_TOP = 0;


    Optional<Exception> isInitialAndpq();

    // Same as getRoles (purely static syntactic) for initial
    Set<Role> getLiveRoles();

    default GTGType unfoldAllOnce() {
        return unfoldAllOnceAux(Set.of());
    }

    // TODO refactor subs is singleton
    @Override
    GTGType subs(RecVar v, GTGRecursion subs);

    GTGType unfoldAllOnceAux(Set<RecVar> recvars);

    Set<Op> getChoiceLabelsUpTo(int c);

    Optional<Exception> checkWellFormed();

    default Optional<Exception> checkedFailedAnnots() {
        return checkedFailedAnnotsAux(Collections.emptySet());
    }

    Optional<Exception> checkedFailedAnnotsAux(Set<Role> failed);

    default Map<Role, Map<Integer, Set<Op>>> getExplicitCommitting() {
        Map<Role, Map<Integer, Set<Op>>> res = new HashMap<>();
        getTimeoutIds().forEach(x -> getExplicitCommitting(x)
                .forEach((k, v) -> res.computeIfAbsent(k, z -> new HashMap<>())
                                      .computeIfAbsent(x, z -> new HashSet<>())
                                      .addAll(v)));
        return res;
    }

    default Map<Role, Set<Op>> getExplicitCommitting(int c) {
        return getExplicitCommittingAux(c, Collections.emptySet());
    }

    Map<Role, Set<Op>> getExplicitCommittingAux(int c, Set<Role> com);

    default Map<Role, Map<Integer, Set<Op>>> getCommittingNew() {
        Map<Role, Map<Integer, Set<Op>>> res = new HashMap<>();
        getTimeoutIds().forEach(x -> getCommittingNew(x)
                .forEach((k, v) -> res.computeIfAbsent(k, z -> new HashMap<>())
                                      .computeIfAbsent(x, z -> new HashSet<>())
                                      .addAll(v)));
        return res;
    }

    default Map<Role, Set<Op>> getCommittingNew(int c) {
        return getCommittingAuxNew(c, Collections.emptySet());
    }

    Map<Role, Set<Op>> getCommittingAuxNew(int c, Set<Role> com);

    Set<Integer> getTimeoutIds();  // c's

    // K depends on V's
    Map<Role, Set<Role>> getStrictSyntacticDeps();

    Map<Role, Set<Role>> getEventualSyntacticDeps();

    boolean isDiverging();

    Set<RecVar> getFreeRecVars();

    // Only uses strict deps
    Optional<Exception> isSyntacticAware();

    // TODO Optional<Exception>
    Optional<Exception> isBalanced();

    default String format() {
        return format("");
    }

    String format(String pref);


    /* ... */

    default Optional<Pair<? extends GTLType, Sigma>> projectTop(Set<Role> topPeers, Role r) {
        return project(topPeers, r, GTLType.c_TOP, GTLType.n_INIT);
    }

    // topPeers for sigma_0
    Optional<Pair<? extends GTLType, Sigma>> project(Set<Role> topPeers, Role r, int c, int n);

    Optional<Theta> projectTheta(Set<Integer> cs, Role r);  // TODO refactor (cf. Theta.project)


    /* ... */

    default Map<Integer, Map<Role, Set<Op>>> getCommitting() {
        Set<Integer> cs = getTimeoutIds();
        return cs.stream().collect(Collectors.toMap(x -> x, this::getCommitting));
    }

    // cf. getTimeoutIds
    default Map<Role, Set<Op>> getCommitting(int c) {
        return getCommittingAux(c, Set.of());
    }

    default Map<Role, Set<Op>> getCommittingAux(int c, Set<Role> com) {
        throw new RuntimeException("TODO");
    }










}
