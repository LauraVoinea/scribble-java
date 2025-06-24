package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.local.*;
import org.scribble.ext.gt.core.type.session.local.runtime.GTLMixedActive;
import org.scribble.ext.gt.util.ConsoleColors;
import org.scribble.ext.gt.util.GTUtil;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class GTGMixedChoice implements GTGType {

    protected final GTGTypeFactory fact = GTGTypeFactory.FACTORY;

    public final int c;       // Currently assigned by GTGTypeTranslator2
    public final Role other;  // other->observer.L |> observer->other.R
    public final Role observer;  // observer?  "monitor"?
    public final GTGType left;  // !!! interaction (other -> obs)
    public final GTGType right;  // !!! interaction (obs -> other)

    public final boolean otherFailedAnnot;

    protected GTGMixedChoice(
            int c, GTGType left, GTGType right, Role other, Role observer, boolean hasFailedAnnot) {
        this.c = c;
        this.other = other;
        this.observer = observer;
        this.left = left;
        this.right = right;

        this.otherFailedAnnot = hasFailedAnnot;
    }

    @Override
    public Optional<Exception> isInitialAndpq() {
        if (!(this.left instanceof GTGInteraction ll && this.right instanceof GTGInteraction rr)) {
            return Optional.of(new Exception("Left/right should be interactions, not: " + this));
        }

        if (!ll.src.equals(this.other) || !ll.dst.equals(this.observer)) {
            return Optional.of(new Exception("Left of (" + this.c + ") expected " +
                    this.other + "->" + this.observer + ", not:\n" + this.left.format()));
        }

        if (!rr.src.equals(this.observer) || !rr.dst.equals(this.other)) {
            return Optional.of(new Exception("Right of (" + this.c + ") expected " +
                    this.observer + "->" + this.other + ", not:\n" + this.right.format()));
        }

        return ll.isInitialAndpq().or(rr::isInitialAndpq);
    }

    @Override
    public Set<Role> getLiveRoles() {
        return GTUtil.union(this.left.getLiveRoles(), this.right.getLiveRoles());
    }

    @Override
    public GTGType unfoldAllOnceAux(Set<RecVar> recvars) {
        return new GTGMixedChoice(this.c, this.left.unfoldAllOnceAux(recvars),
                this.right.unfoldAllOnceAux(recvars), this.other, this.observer, this.otherFailedAnnot);
    }

    @Override
    public Set<Op> getChoiceLabelsUpTo(int c) {
        if (this.c == c) {
            return Collections.emptySet();
        } else {
            Set<Op> res = new HashSet<>();
            res.addAll(this.left.getChoiceLabelsUpTo(c));
            res.addAll(this.right.getChoiceLabelsUpTo(c));
            return res;
        }
    }

    @Override
    public Optional<Exception> checkWellFormed() {
        Set<Op> lleft = this.left.getChoiceLabelsUpTo(this.c);
        Set<Op> lright = this.right.getChoiceLabelsUpTo(this.c);
        lleft.retainAll(lright);
        if (!lleft.isEmpty()) {
            return Optional.of(new Exception("Not well formed: labels left=" +
                    lleft + ", right=" + lright + " not disjoint up to ("
                    + this.c + ") in:\n" + this.format()));
        } else {
            return this.left.checkWellFormed().or(this.right::checkWellFormed);
        }
    }

    @Override
    public Optional<Exception> checkedFailedAnnotsAux(Set<Role> failed) {
        return this.left.checkedFailedAnnotsAux(failed)
                        .or(() -> {
                            if (this.otherFailedAnnot) {
                                Set<Role> tmp = new HashSet<>(failed);
                                tmp.add(this.other);
                                GTGInteraction right = (GTGInteraction) this.right;
                                return right.cases.values().stream()
                                                  .flatMap(x -> x.checkedFailedAnnotsAux(tmp).stream())
                                                  .findAny();
                            } else {
                                return this.right.checkedFailedAnnotsAux(failed);
                            }
                        });
    }

    @Override
    public Map<Role, Set<Op>> getExplicitCommittingAux(int c, Set<Role> com) {
        if (c == this.c) {
            Map<Role, Set<Op>> res = new HashMap<>();

            Set<Op> obs = new HashSet<>();
            // obs not implicitly com on LHS
            obs.addAll(((GTGInteraction) this.right).cases.keySet());
            res.put(this.observer, obs);
            Set<Op> oth = new HashSet<>();
            oth.addAll(((GTGInteraction) this.right).cases.keySet());
            res.put(this.other, oth);

            Map<Role, Set<Op>> ll = this.left.getExplicitCommittingAux(c, com);  // obs not implicitly com on LHS
            ll.forEach((k, v) -> res.computeIfAbsent(k, x -> new HashSet<>()).addAll(v));

            Set<Role> r = new HashSet<>(com);
            r.add(this.observer);
            r.add(this.other);
            Map<Role, Set<Op>> rr = this.right.getExplicitCommittingAux(c, r);
            rr.forEach((k, v) -> res.computeIfAbsent(k, x -> new HashSet<>()).addAll(v));

            return res;
        } else {
            Map<Role, Set<Op>> res = new HashMap<>();
            Map<Role, Set<Op>> ll = this.left.getExplicitCommittingAux(c, com);
            ll.forEach((k, v) -> res.computeIfAbsent(k, x -> new HashSet<>()).addAll(v));
            Map<Role, Set<Op>> rr = this.right.getExplicitCommittingAux(c, com);
            rr.forEach((k, v) -> res.computeIfAbsent(k, x -> new HashSet<>()).addAll(v));
            return res;
        }
    }

    @Override
    public Map<Role, Set<Op>> getCommittingAuxNew(int c, Set<Role> com) {
        if (c == this.c) {
            Map<Role, Set<Op>> res = new HashMap<>();

            Set<Op> obs = new HashSet<>();
            obs.addAll(((GTGInteraction) this.left).cases.keySet());
            obs.addAll(((GTGInteraction) this.right).cases.keySet());
            res.put(this.observer, obs);
            Set<Op> oth = new HashSet<>();
            oth.addAll(((GTGInteraction) this.right).cases.keySet());
            res.put(this.other, oth);

            Set<Role> l = new HashSet<>(com);
            l.add(this.observer);
            Map<Role, Set<Op>> ll = this.left.getCommittingAuxNew(c, l);  // Should be visiting continuations, but doesn't hurt to start top of left
            ll.forEach((k, v) -> res.computeIfAbsent(k, x -> new HashSet<>()).addAll(v));
            Set<Role> r = new HashSet<>(com);
            r.add(this.observer);
            r.add(this.other);

            Map<Role, Set<Op>> rr = this.right.getCommittingAuxNew(c, r);  // Should be visiting continuations, but doesn't hurt
            rr.forEach((k, v) -> res.computeIfAbsent(k, x -> new HashSet<>()).addAll(v));

            return res;
        } else {
            Map<Role, Set<Op>> res = new HashMap<>();
            Map<Role, Set<Op>> ll = this.left.getCommittingAuxNew(c, com);
            ll.forEach((k, v) -> res.computeIfAbsent(k, x -> new HashSet<>()).addAll(v));
            Map<Role, Set<Op>> rr = this.right.getCommittingAuxNew(c, com);
            rr.forEach((k, v) -> res.computeIfAbsent(k, x -> new HashSet<>()).addAll(v));
            return res;
        }
    }

    @Override
    public Set<Integer> getTimeoutIds() {
        Set<Integer> res = new HashSet<>();
        res.add(this.c);
        res.addAll(this.left.getTimeoutIds());
        res.addAll(this.right.getTimeoutIds());
        return res;
    }

    @Override
    public Map<Role, Set<Role>> getStrictSyntacticDeps() {
        Map<Role, Set<Role>> l = this.left.getStrictSyntacticDeps();
        Map<Role, Set<Role>> r = this.right.getStrictSyntacticDeps();
        return GTGInteraction.mergeSyntacticDeps(l, r);
    }

    @Override
    public Map<Role, Set<Role>> getEventualSyntacticDeps() {
        Map<Role, Set<Role>> l = this.left.getEventualSyntacticDeps();
        Map<Role, Set<Role>> r = this.right.getEventualSyntacticDeps();
        return GTGInteraction.mergeSyntacticDeps(l, r);
    }

    @Override
    public boolean isDiverging() {
        return this.left.isDiverging() && this.right.isDiverging();
    }

    @Override
    public Set<RecVar> getFreeRecVars() {
        return Stream.concat(
                this.left.getFreeRecVars().stream(),
                this.right.getFreeRecVars().stream()
        ).collect(Collectors.toSet());
    }

    @Override
    public Optional<Exception> isSyntacticAware() {
        Optional<Exception> nested = this.left.isSyntacticAware().or(this.right::isSyntacticAware);
        if (nested.isPresent()) {
            return nested;
        }

        Set<Role> rs = new HashSet<>(getLiveRoles());
        rs.remove(this.observer);  // !!!
        return isSyntacticClearTermination(rs).or(() -> isSyntacticSingleDecision(rs));
    }

    // !!! "clear termination" name, cf. RHS diverging
    protected Optional<Exception> isSyntacticClearTermination(Set<Role> rs) {
        if (this.left.isDiverging() &&
                (this.left.getFreeRecVars().isEmpty() || this.right.getFreeRecVars().isEmpty())) {
            return Optional.empty();
        }
        Map<Role, Set<Role>> ledeps = this.left.getEventualSyntacticDeps();
        return rs.stream()
                 .filter(x -> !ledeps.containsKey(x) || !ledeps.get(x).contains(this.observer))
                 .findAny()
                 .map(x -> new Exception("Not left committing for " + x + " in:\n" + this.format()));
    }

    protected Optional<Exception> isSyntacticSingleDecision(Set<Role> rs) {
        Map<Role, Set<Role>> rsdeps = this.right.getStrictSyntacticDeps();
        return rs.stream()
                 .filter(x -> !rsdeps.containsKey(x) || !rsdeps.get(x).contains(this.observer))
                 .findAny()
                 .map(x -> new Exception("Not right committing for " + x +
                         " in " + this.c + ":\n" + this.format()));
    }

    @Override
    public Optional<Exception> isBalanced() {
        Set<Role> ll = this.left.getLiveRoles();
        Set<Role> rr = this.right.getLiveRoles();
        return ll.equals(rr)
               ? Optional.empty()
               : Optional.of(new Exception("Not balanced left=" + ll + ", right=" + rr + " in:\n" + this.format()));
    }

    @Override
    public String format(String pref) {
        String res = pref + "(" +
                "\n" + this.left.format(pref + "    ") +
                "\n" + pref + ConsoleColors.WHITE_TRIANGLE + this.c + ":" + this.other + "," + this.observer +
                "\n" + this.right.format(pref + "    ") +
                "\n" + pref + ")";
        return ConsoleColors.getMCColour(this.c) + res + ConsoleColors.RESET;
    }










    // OLD

    /* ... */

    @Override
    public boolean isInitialWellSet(Set<Integer> cs) {
        if (!(this.left instanceof GTGInteraction) || !(this.right instanceof GTGInteraction)) {
            return false;
        }
        if (cs.contains(this.c)) {
            return false;
        }
        Set<Integer> copy = GTUtil.copyOf(cs);
        copy.add(this.c);
        GTGInteraction left = (GTGInteraction) this.left;
        GTGInteraction right = (GTGInteraction) this.right;
        return left.isInitialWellSet(copy) && right.isInitialWellSet(copy)
                && left.getLiveRoles().equals(right.getLiveRoles())  // timeout participation
                && this.other.equals(left.getSender()) && this.other.equals(right.getReceiver())
                && this.observer.equals(left.getReceiver()) && this.observer.equals(right.getSender());
    }

    @Override
    public Map<Role, Set<Role>> getStrongDeps() {
        Map<Role, Set<Role>> left = this.left.getStrongDeps();
        Map<Role, Set<Role>> right = this.right.getStrongDeps();
        Set<Role> rs = this.getLiveRoles();
        rs.remove(this.other);
        rs.remove(this.observer);
        Map<Role, Set<Role>> res = GTUtil.mapOf();
        for (Role r : rs) {
            if (!left.containsKey(r) || !right.containsKey(r)) {
                res.put(r, GTUtil.setOf());
                continue;
            }
            Set<Role> tmp = left.get(r);
            tmp.retainAll(right.get(r));
            res.put(r, tmp);
        }
        return res;
    }

    @Override
    public boolean isSingleDecision(Set<Role> topAll, Theta theta) {
        Map<Role, Set<Role>> right = this.right.getStrongDeps();
        Set<Role> rs = this.getLiveRoles();
        rs.removeAll(getIndifferent(topAll));
        rs.remove(this.observer);  // !!! CHECKME
        for (Role r : rs) {

            if (!right.containsKey(r) || !right.get(r).contains(this.observer)) {  // only single-decision -- !!! clear-termination approx by isLeftCommitting
                return false;
            }
        }

        //System.out.println("[Warning] TODO weak-dependencies and clear-termination: " + this);  // cf. isLeftCommitting

        return this.left.isSingleDecision(topAll, theta) && this.right.isSingleDecision(topAll, theta);
    }

    @Override
    public boolean isClearTermination() {
        //return isLeftCommitting(GTUtil.setOf(), getRoles());  // n.b., roles(this) -- "outer" roles not involved at all don't matter
        return this.left.isClearTerminationAux(this.observer, GTUtil.setOf(), this.getLiveRoles())  // n.b., roles(this) -- "outer" roles not involved at all don't matter
                && this.left.isClearTermination()
                && this.right.isClearTermination();
    }

    @Override
    public boolean isClearTerminationAux(Role obs, Set<Role> com, Set<Role> rem) {
        return this.left.isClearTerminationAux(obs, com, rem)
                && this.right.isClearTerminationAux(obs, com, rem);
    }


    /* ... */

    @Override
    public Optional<Pair<? extends GTLType, Sigma>> project(Set<Role> topPeers, Role r, int c, int n) {
        GTLTypeFactory lf = GTLTypeFactory.FACTORY;

        Optional<Pair<? extends GTLType, Sigma>> optl = this.left.project(topPeers, r, c, n);
        Optional<Pair<? extends GTLType, Sigma>> optr = this.right.project(topPeers, r, c, n);
        //System.out.println("XXXXXX " + r + ": " + optl + " ,, " + optr);
        if (optl.isEmpty() || optr.isEmpty()) { return Optional.empty(); }
        Pair<? extends GTLType, Sigma> get_l = optl.get();
        Pair<? extends GTLType, Sigma> get_r = optr.get();

        Set<Role> top = GTUtil.union(GTUtil.copyOf(topPeers), Set.of(r));
        Set<Role> indiff = getIndifferent(top);
        if (indiff.contains(r)) {
            return get_l.equals(get_r) ? optl : Optional.empty();
        }

        // else r not indiff

        Sigma s0 = new Sigma(topPeers);
        if (!s0.equals(get_l.right) || !s0.equals(get_r.right)) {
            return Optional.empty();
        }

        /*return !r.equals(this.other) && !r.equals(this.observer)
                ? get_l.left.merge(get_r.left).map(x -> Pair.of(x, s0))  // !!! refactor with GTGInteraction.merge
                : Optional.of(new Pair<>(lf.mixedChoice(this.c, get_l.left, get_r.left), s0));*/
//        HERE HERE XXX could be either white triangle or transparent -merge
//        depending on I / O in full generality ? (white triangle merge
//        only definitely static initial)similarly for black triangle
//
//        cf.regular MPST choice:
//        unlike MC, regular choice not retained as "syntactic context", so
//        the dynamic "merging" between fluctuating I / O doesn 't occur there

        /*if (r.equals(this.other) || r.equals(this.observer)) {
            return Optional.of(Pair.of(lf.mixedChoice(this.c, get_l.left, get_r.left), s0));
        } else {
            // TODO conditions? -- abstract global props should be implemented here?

            if (isMergableIOModes(get_l.left, get_r.left)) {
                return get_l.left.merge(get_r.left).map(x -> Pair.of(x, s0));  // !!! refactor with GTGInteraction.merge
            } else {

                // FIXME TODO conditions corresponding to global props?
                // cf. (old) single-pointed
                Set<Op> ops = this.left.getOps();
                ops.retainAll(this.right.getOps());
                if (!ops.isEmpty()) {
                    return Optional.empty();
                }
                if (!(this.left instanceof GTLBranch && this.right instanceof GTLSelect)
                        || !(this.left instanceof GTLSelect && this.right instanceof GTLBranch)) {
                    return Optional.empty();
                }
                if (!getPeer(get_l.left).equals(get_r.left)) {  // CHECKME currently no recursive check (cf. single-pointed, also merge)
                    return Optional.empty();
                }

                return Optional.of(Pair.of(lf.mixedChoice(this.c, get_l.left, get_r.left), s0));
            }
        }*/

//        // theory doesn't use merge in white triangle projection...
//        if (!r.equals(this.other) && !r.equals(this.observer)) {
//
//            // HERE HERE FIXME need to distinguish I/O cases (merge vs. MC) -- e.g., MC with third-party receiving from different peers
//
//            if (isMergableIOModes(get_l.left, get_r.left)) {  // "modes"
//                Optional<? extends GTLType> merge = get_l.left.merge(get_r.left);
//                System.out.println("XXXXXX " + r + " " + merge);
//                if (merge.isEmpty()) {
//                    return Optional.empty();
//                }
//
//            } else {
//                // TODO FIXME MC conditions?
//                //global protocol Proto1(role A, role B, role C) { mixed { 1() from A to C;mixed { 2() from A to B;3() from A to C;a() from B to A;b() from B to C;e() from C to A;f() from C to B; } () or A -> B () { 4() from B to A;c() from B to C;d() from C to A;g() from C to B; } } () or A -> C () { 5() from C to A;5() from C to B; }}
//                //global protocol Proto1(role A, role B, role C) { mixed { 1() from A to C;2() from C to A;/*3() from C to B;*/mixed { a() from A to B;b() from B to A;c() from B to C;3a() from C to B; } () or A -> B () { d() from B to A;e() from B to C;3b() from C to B; } } () or A -> C () { 4() from C to A;5() from C to B; } }
//                throw new RuntimeException("XXXXXX " + r + " TODO " + get_l.left + ", " + getMode(get_l.left) + "\n " + get_r.left + ", " + getMode(get_r.left));
//            }
//        }
        return Optional.of(Pair.of(lf.mixedChoice(this.c, get_l.left, get_r.left), s0));
    }

    /*protected static Role getPeer(GTLType t) {
        if (t instanceof GTLBranch) {
            return ((GTLBranch) t).src;
        } else if (t instanceof GTLSelect) {
            return ((GTLSelect) t).dst;
        } else {
            throw new RuntimeException("Shouldn't get here: " + t);
        }
    }*/

    public static boolean isMergableIOModes(GTLType left, GTLType right) {
        IOMode m_left = getMode(left);
        IOMode m_right = getMode(right);
        return m_left == m_right
                && m_left != IOMode.MIXED
                && m_left != IOMode.REC;  // TODO
    }

    // TODO refactor
    protected enum IOMode {
        IN,
        OUT,
        MIXED,
        END,
        REC  // !!! TODO CHECKME merge for rec -- should be "transparent" ?
    }

    protected static IOMode getMode(GTLType t) {
        if (t instanceof GTLBranch) {
            return IOMode.IN;
        } else if (t instanceof GTLSelect) {
            return IOMode.OUT;
        } else if (t instanceof GTLMixedChoice || t instanceof GTLMixedActive) {
            return IOMode.MIXED;
        } else if (t instanceof GTLRecursion) {
            //return getMode(((GTLRecursion) t).body);
            return IOMode.REC;
        } else if (t instanceof GTLEnd) {
            return IOMode.END;
        } else {
            throw new RuntimeException("Shouldn't get here: " + t);
        }
    }

    @Override
    public Optional<Theta> projectTheta(Set<Integer> cs, Role r) {
        return Optional.of(new Theta(cs));
    }


    /* ... */

    @Override
    public Map<Role, Set<Op>> getCommittingAux(int c, Set<Role> com) {
        GTGInteraction left = (GTGInteraction) this.left;
        GTGInteraction right = (GTGInteraction) this.right;
        Map<Role, Set<Op>> res = new HashMap<>();
        if (c == this.c) {
            Set<Role> tmp1 = new HashSet<>(com);
            tmp1.add(this.observer);
            for (GTGType x : left.cases.values()) {
                for (Map.Entry<Role, Set<Op>> y : x.getCommittingAux(c, tmp1).entrySet()) {
                    Set<Op> ops = res.computeIfAbsent(y.getKey(), z -> new HashSet<>());
                    ops.addAll(y.getValue());
                }
            }

            Set<Role> tmp2 = new HashSet<>(tmp1);
            tmp2.add(this.other);
            for (GTGType x : right.cases.values()) {
                for (Map.Entry<Role, Set<Op>> y : x.getCommittingAux(c, tmp2).entrySet()) {
                    Set<Op> ops = res.computeIfAbsent(y.getKey(), z -> new HashSet<>());
                    ops.addAll(y.getValue());
                }
            }

            Set<Op> obs = res.computeIfAbsent(this.observer, x -> new HashSet<>());
            obs.addAll(left.cases.keySet());
            obs.addAll(right.cases.keySet());
            Set<Op> other = res.computeIfAbsent(this.other, x -> new HashSet<>());
            other.addAll(right.cases.keySet());

            return res;
        } else {
            for (Map.Entry<Role, Set<Op>> x : this.left.getCommittingAux(c, com).entrySet()) {
                Set<Op> ops = res.computeIfAbsent(x.getKey(), y -> new HashSet<>());
                ops.addAll(x.getValue());
            }
            for (Map.Entry<Role, Set<Op>> x : this.right.getCommittingAux(c, com).entrySet()) {
                Set<Op> ops = res.computeIfAbsent(x.getKey(), y -> new HashSet<>());
                ops.addAll(x.getValue());
            }
            return res;
        }
    }

    // ...

    @Override
    public Map<Role, Set<Op>> getCommittingTop(Set<Role> com) {
        /*Set<Op> res = this.left.getCommittingLeft(this.observer, com);
        res.addAll(this.right.getCommittingRight(this.observer, com));*/
        //Set<Op> res = this.left.getCommittingLeft(this.observer, GTUtil.setOf());
        //res.addAll(this.right.getCommittingRight(this.observer, GTUtil.setOf()));
        Map<Role, Set<Op>> res = this.left.getCommittingLeft(this.observer, GTUtil.setOf());
        this.right.getCommittingRight(this.observer, GTUtil.setOf()).forEach((k, v) -> {
            Set<Op> ops = res.computeIfAbsent(k, y -> new HashSet<>());
            ops.addAll(v);
        });
        return res;
    }

    @Override
    public Map<Role, Set<Op>> getCommittingLeft(Role obs, Set<Role> com) {
        //return getCommittingTop();
        return getCommittingTop(com);
    }

    @Override
    public Map<Role, Set<Op>> getCommittingRight(Role obs, Set<Role> com) {
        //return getCommittingTop();
        return getCommittingTop(com);
    }

    @Override
    public Pair<Set<Op>, Map<Integer, Pair<Set<Op>, Set<Op>>>> getLabels() {
        Pair<Set<Op>, Map<Integer, Pair<Set<Op>, Set<Op>>>> l = this.left.getLabels();
        Pair<Set<Op>, Map<Integer, Pair<Set<Op>, Set<Op>>>> r = this.right.getLabels();
        Map<Integer, Pair<Set<Op>, Set<Op>>> res = GTUtil.copyOf(l.right);
        if (res.keySet().stream().anyMatch(x -> r.right.containsKey(x))) {
            throw new RuntimeException("Shouldn't get here: " + l + " ,," + r);
        }
        res.putAll(r.right);

        // FIXME merge across nested MCs? cf. TODO merge for MC

        if (res.containsKey(this.c)) {
            throw new RuntimeException("Shouldn't get here: " + l + " ,," + r);
        }

        // CHECKME dropping mergable labs
        Set<Op> l1 = GTUtil.copyOf(l.left);
        Set<Op> r1 = GTUtil.copyOf(r.left);
        l1.removeAll(r.left);
        r1.removeAll(l.left);
        res.put(this.c, Pair.of(l1, r1));
        return Pair.of(GTUtil.setOf(), res);
    }


    /* Aux */

    @Override
    public GTGMixedChoice subs(RecVar v, GTGRecursion subs) {
        GTGType left = this.left.subs(v, subs);
        GTGType right = this.right.subs(v, subs);
        return new GTGMixedChoice(this.c, left, right, this.other, this.observer, this.otherFailedAnnot);
    }

    @Override
    public Set<Role> getReadyAux(Set<Role> blocked) {

        // !!! here doing a "weak" version, implicitly bypassing \nu -- cf. def 7 ready (and lemma 3 LR-initiation, quantified over reachable)

        Set<Role> res = this.left.getReadyAux(blocked);
        res.addAll(this.right.getReadyAux(blocked));  // Should be just this.observer
        return res;
    }

    @Override
    public Set<Op> getOps() {
        Set<Op> ops = new HashSet<>(this.left.getOps());
        ops.addAll(this.right.getOps());
        return ops;
    }

    @Override
    public Set<RecVar> getRecDecls() {
        return GTUtil.union(
                this.left.getRecDecls(),
                this.right.getRecDecls());
    }

    @Override
    public String toString() {
        return ConsoleColors.toMixedChoiceString("(" + this.left)
                + ConsoleColors.toMixedChoiceString(" " + ConsoleColors.WHITE_TRIANGLE  // XXX not fully working, cf. ConsoleColors reset and nested
                + this.c + ":" + this.other + (this.otherFailedAnnot ? "@failed" : "") + "," + this.observer
                + " " + this.right)
                + ConsoleColors.toMixedChoiceString(")");
    }


    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTGType.GLOBAL_MIXED_DEF_HASH;
        hash = 31 * hash + this.c;
        hash = 31 * hash + this.left.hashCode();
        hash = 31 * hash + this.right.hashCode();
        hash = 31 * hash + this.other.hashCode();
        hash = 31 * hash + this.observer.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) { return true; }
        if (obj == null || !(obj instanceof GTGMixedChoice)) { return false; }
        GTGMixedChoice them = (GTGMixedChoice) obj;
        return them.canEquals(this)
                && this.c == them.c
                && this.left.equals(them.left)
                && this.right.equals(them.right)
                && this.other.equals(them.other)
                && this.observer.equals(them.observer);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTGMixedChoice;
    }

















    /* ... */

    // Dup with GTGMixedActive  // TODO factor out
    public Set<Role> getIndifferent(Set<Role> top) {
        Set<Role> rs = this.getLiveRoles();
        Set<Role> copy = GTUtil.copyOf(rs);
        copy.remove(this.other);
        copy.remove(this.observer);
        // !!! conservative? -- CHECKME does that affect safety w.r.t. static awareness?
        return rs.stream().filter(x ->
                         //this.left.projectTop(top, x).equals(this.right.projectTop(top, x)))
                 {
                     Optional<Pair<? extends GTLType, Sigma>> o_l = this.left.projectTop(top, x);
                     Optional<Pair<? extends GTLType, Sigma>> o_r = this.right.projectTop(top, x);
                     Optional<Boolean> res = o_l.flatMap(y -> o_r.map(z -> y.left.equals(z.left)));  // !!! only w.r.t. type -- cf. regular/wiggly indiff (non equal queues)
                     return res.isPresent() && res.get();
                 })
                 .collect(Collectors.toSet());
    }






















    /* ... */

    @Override
    public boolean isRuntimeChoicePartip() {
        return this.left.isRuntimeChoicePartip() && this.right.isRuntimeChoicePartip();  // XXX CHECKME (cf. merge third parties)
    }

    @Override
    public boolean isUniqueInstan(Set<Pair<Integer, Integer>> seen) {
        // !!! morally can prune if starting from initial
        return this.left.isUniqueInstan(seen) && this.right.isUniqueInstan(seen);
    }

    @Override
    public boolean isAwareCorollary(GTSModelFactory mf, Set<Role> topAll, Theta theta) {
        // Can morally just return true
        return this.left.isAwareCorollary(mf, topAll, theta) && this.right.isAwareCorollary(mf, topAll, theta);
    }

    @Override
    public boolean isCoherent() {
        // Morally can just return true
        return this.left.isCoherent() && this.right.isCoherent();
    }


    /* ... */

    @Override
    public GTGMixedChoice unfoldAllImmediateRecs() {
        return this;
    }










}
