package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.name.GTOp;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.core.type.session.local.GTLTypeFactory;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

// !!! FIXME naming "interaction" vs. "choice" (in other places)
public class GTGInteraction implements GTGType {

    public final Role src;
    public final Role dst;
    public final Map<Op, Payload> pays;  // Pre: Unmodifiable -- keyset subset of cases; values non-null
    public final Map<Op, GTGType> cases;  // Pre: "Ordered", Unmodifiable, non-empty

    protected GTGInteraction(Role src, Role dst, LinkedHashMap<Op, Payload> pays, LinkedHashMap<Op, GTGType> cases) {
        this.src = src;
        this.dst = dst;
        this.pays = Collections.unmodifiableMap(pays.entrySet().stream().collect(
                Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new)));
        this.cases = Collections.unmodifiableMap(cases.entrySet().stream().collect(
                Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new)));
    }

    @Override
    public Optional<Exception> isInitialAndpq() {
        return this.cases.values().stream()
                         .map(GTGType::isInitialAndpq)
                         .filter(Optional::isPresent)
                         .findAny()
                         .orElse(Optional.empty());
    }

    @Override
    public Set<Role> getLiveRoles() {
        return Stream.concat(Stream.of(this.src, this.dst),
                             this.cases.values().stream().flatMap(x -> x.getLiveRoles().stream()))
                     .collect(Collectors.toSet());
    }

    @Override
    public GTGType unfoldAllOnceAux(Set<RecVar> recvars) {
        LinkedHashMap<Op, GTGType> nested = this.cases.entrySet().stream().collect(
                Collectors.toMap(
                        Map.Entry::getKey,
                        x -> x.getValue().unfoldAllOnceAux(recvars),
                        (x, y) -> null,
                        LinkedHashMap::new
                ));
        return new GTGInteraction(this.src, this.dst, new LinkedHashMap<>(this.pays), nested);
    }

    @Override
    public Set<Op> getChoiceLabelsUpTo(int c) {
        Set<Op> nested = this.cases.values().stream().flatMap(x ->
                x.getChoiceLabelsUpTo(c).stream()).collect(Collectors.toSet());
        Set<Op> res = new HashSet<>();
        res.addAll(this.cases.keySet());
        res.addAll(nested);
        return res;
    }

    @Override
    public Optional<Exception> checkWellFormed() {
        for (GTGType x : this.cases.values()) {
            Optional<Exception> y = x.checkWellFormed();
            if (y.isPresent()) {
                return y;
            }
        }
        return Optional.empty();
    }

    @Override
    public Optional<Exception> checkedFailedAnnotsAux(Set<Role> failed) {
        if (failed.contains(this.src)) {
            return Optional.of(new Exception("Cannot use failed role " + this.src + ": " + this));
        }
        if (failed.contains(this.dst)) {
            return Optional.of(new Exception("Cannot use failed role " + this.dst + ": " + this));
        }
        return this.cases.values().stream().flatMap(x -> x.checkedFailedAnnotsAux(failed).stream()).findAny();
    }

    @Override
    public Map<Role, Set<Op>> getExplicitCommittingAux(int c, Set<Role> com) {
        Map<Role, Set<Op>> res = new HashMap<>();
        Set<Role> tmp = com;
        if (!com.contains(this.dst) && com.contains(this.src)) {
            tmp = new HashSet<>(com);
            tmp.add(this.dst);
            res.put(this.dst, this.cases.keySet());
        } else {
            res.put(this.dst,
                    this.cases.keySet().stream()
                              .filter(x -> ((GTOp) x).annots.contains(GTOp.EXPLICIT_COMMIT))
                              .collect(Collectors.toSet()));
        }
        /*for (GTGType x : this.cases.values()) {
            x.getExplicitCommittingAux(c, tmp).forEach((k, v) ->
                    res.computeIfAbsent(k, z -> new HashSet<>()).addAll(v));
        }*/
        for (Map.Entry<Op, GTGType> e : this.cases.entrySet()) {
            Set<Role> tmp2 = new HashSet<>(tmp);
            if (((GTOp) e.getKey()).annots.contains(GTOp.EXPLICIT_COMMIT)) {
                tmp2.add(this.dst);
            }
            e.getValue().getExplicitCommittingAux(c, tmp2).forEach((k, v) ->
                    res.computeIfAbsent(k, z -> new HashSet<>()).addAll(v));
        }

        return res;
    }

    @Override
    public Map<Role, Set<Op>> getCommittingAuxNew(int c, Set<Role> com) {
        Map<Role, Set<Op>> res = new HashMap<>();
        Set<Role> tmp = com;
        if (!com.contains(this.dst) && com.contains(this.src)) {
            tmp = new HashSet<>(com);
            tmp.add(this.dst);
            res.put(this.dst, this.cases.keySet());
        }
        for (GTGType x : this.cases.values()) {
            x.getCommittingAuxNew(c, tmp).forEach((k, v) ->
                    res.computeIfAbsent(k, z -> new HashSet<>()).addAll(v));
        }
        return res;
    }

    @Override
    public Set<Integer> getTimeoutIds() {
        return this.cases.values().stream()
                         .flatMap(x -> x.getTimeoutIds().stream())
                         .collect(Collectors.toSet());
    }

    @Override
    public Map<Role, Set<Role>> getStrictSyntacticDeps() {
        Map<Role, Set<Role>> nested = new HashMap<>(
                this.cases.values().stream()
                          .map(GTGType::getStrictSyntacticDeps)
                          .reduce(GTGInteraction::mergeSyntacticDeps).get());  // Pre: non-empty cases

        Map<Role, Set<Role>> copy = new HashMap<>(nested);
        copy.put(this.src, Collections.emptySet());

        for (Map.Entry<Role, Set<Role>> x : nested.entrySet()) {
            Role k = x.getKey();
            Set<Role> vs = x.getValue();
            if (!k.equals(this.src) && vs.contains(this.dst)) {
                Set<Role> tmp = new HashSet<>(vs);
                tmp.add(this.src);
                copy.put(k, tmp);
            }
        }

        Set<Role> curr = new HashSet<>(copy.getOrDefault(this.dst, Collections.emptySet()));
        curr.add(this.src);
        copy.put(this.dst, curr);
        return copy;
    }

    // !!! syntactic deps relies on RHS awareness -- RHS can happen any time (i.e., before an LHS committing action), but single-decision ensures all aware
    @Override
    public Map<Role, Set<Role>> getEventualSyntacticDeps() {
        boolean allDiv = this.cases.values().stream().allMatch(GTGType::isDiverging);
        Map<Role, Set<Role>> nested =
                this.cases.values().stream()

                          // !!! OK because _eventual_ can be freely past or future (cf. strict)
                          // !!! TODO could also relax MC left/right if diverging ?
                          .filter(x -> allDiv || !x.isDiverging())

                          .map(GTGType::getEventualSyntacticDeps)  // !!! eventual
                          .reduce(GTGInteraction::mergeSyntacticDeps).get();  // Pre: non-empty

        Map<Role, Set<Role>> copy = new HashMap<>(nested);
        //copy.put(this.src, Collections.emptySet());  // !!! eventual

        for (Map.Entry<Role, Set<Role>> x : nested.entrySet()) {
            Role k = x.getKey();
            Set<Role> vs = x.getValue();
            if (vs.contains(this.dst)) {  // !!! eventual
                Set<Role> tmp = new HashSet<>(vs);
                tmp.add(this.src);
                copy.put(k, tmp);
            }
        }

        Set<Role> tmp = copy.getOrDefault(this.dst, Collections.emptySet());
        Set<Role> curr = new HashSet<>(tmp);
        curr.add(this.src);
        copy.put(this.dst, curr);

        return copy;
    }

    protected static Map<Role, Set<Role>> mergeSyntacticDeps(
            Map<Role, Set<Role>> x, Map<Role, Set<Role>> y) {
        Set<Role> ks = new HashSet<>(x.keySet());
        ks.addAll(y.keySet());
        return ks.stream().collect(Collectors.toMap(
                k -> k,
                k -> {
                    Set<Role> vs = x.getOrDefault(k, Collections.emptySet());
                    vs.retainAll(y.getOrDefault(k, Collections.emptySet()));
                    return vs;
                }));
    }

    @Override
    public boolean isDiverging() {
        return this.cases.values().stream().allMatch(GTGType::isDiverging);
    }

    @Override
    public Set<RecVar> getFreeRecVars() {
        return this.cases.values().stream()
                         .flatMap(x -> x.getFreeRecVars().stream())
                         .collect(Collectors.toSet());
    }

    @Override
    public Optional<Exception> isSyntacticAware() {
        return this.cases.values().stream()
                         .map(GTGType::isSyntacticAware)
                         .filter(Optional::isPresent)
                         .findFirst()
                         .orElseGet(Optional::empty);
    }

    @Override
    public Optional<Exception> isBalanced() {
        Set<Role> pq = Set.of(this.src, this.dst);
        Set<Set<Role>> rs =
                this.cases.values().stream()
                          .map(GTGType::getLiveRoles)
                          .map(x -> x.stream().filter(y -> !pq.contains(y)).collect(Collectors.toSet()))
                          .collect(Collectors.toSet());
        return rs.size() == 1
               ? Optional.empty()
               : Optional.of(new Exception("Choice cases not balanced " + rs +
                       " in:\n" + this.format()));
    }

    @Override
    public String format(String pref) {
        String res = pref + this.src + " -> " + this.dst;
        if (this.cases.size() == 1) {
            Map.Entry<Op, GTGType> x = this.cases.entrySet().iterator().next();
            Op op = x.getKey();
            Payload pay = this.pays.get(op);
            return res + " " + msgToString(op, pay) + "." +
                    "\n" + x.getValue().format(pref);
        } else {
            return res + "{" +
                    this.cases.entrySet().stream()
                              .map(e -> "\n" + pref + "    " + msgToString(e.getKey()) + "." +
                                      "\n" + e.getValue().format(pref + "    "))
                              .collect(Collectors.joining(", ")) +
                    "\n" + pref + "}";
        }
    }


    /* ... */

    @Override
    public Optional<Pair<? extends GTLType, Sigma>> project(Set<Role> topPeers, Role r, int c, int n) {
        GTLTypeFactory lf = GTLTypeFactory.FACTORY;
        if (r.equals(this.src) || r.equals(this.dst)) {
            LinkedHashMap<Op, GTLType> cases = new LinkedHashMap<>();
            Sigma sigma = null;
            for (Map.Entry<Op, GTGType> e : this.cases.entrySet()) {
                Optional<Pair<? extends GTLType, Sigma>> opt = e.getValue().project(topPeers, r, c, n);
                if (opt.isEmpty()) {
                    return Optional.empty();
                }

                // TODO factor out with merge case (reduce over sigmas)
                Pair<? extends GTLType, Sigma> p = opt.get();
                if (sigma == null) {
                    sigma = p.right;
                } else if (!sigma.equals(p.right)) {
                    return Optional.empty();
                }

                Op op = e.getKey();
                cases.put(op, p.left);
            }
            return r.equals(this.src)
                   ? Optional.of(new Pair<>(lf.select(this.dst, new LinkedHashMap<>(this.pays), cases), sigma))
                   : Optional.of(new Pair<>(lf.branch(this.src, new LinkedHashMap<>(this.pays), cases), sigma));
        } else {
            Stream<Optional<Pair<? extends GTLType, Sigma>>> str =
                    this.cases.values().stream().map(x -> x.project(topPeers, r, c, n));
            Optional<Pair<? extends GTLType, Sigma>> fst = str.findFirst().get();  // Non-empty

            // FIXME stream made twice... -- refactor with GTGWiggly
            str = this.cases.values().stream().map(x -> x.project(topPeers, r, c, n));  // !!! XXX
            return str.skip(1).reduce(fst, GTGInteraction::mergePair);
        }
    }

    @Override
    public Optional<Theta> projectTheta(Set<Integer> cs, Role r) {
        if (this.src.equals(r) || this.dst.equals(r)) {
            return Optional.of(new Theta(cs));
        }
        // FIXME refactor merge
        List<Optional<Theta>> distinct = this.cases.values().stream()
                                                   .map(x -> x.projectTheta(cs, r)).distinct().collect(Collectors.toList());
        if (distinct.size() != 1) {
            return Optional.empty();
        }
        return distinct.get(0);
    }


    /* ... */

    // TODO refactor with GTMixedActive -- XXX mixed active needs to do Sigma.circ
    public static Optional<Pair<? extends GTLType, Sigma>> mergePair(
            Optional<Pair<? extends GTLType, Sigma>> left,
            Optional<Pair<? extends GTLType, Sigma>> right) {
        /*if (left.isEmpty() || right.isEmpty()) {
            return Optional.empty();
        }*/
        Optional<? extends GTLType> merge = mergeSyntacticDeps(left.map(x -> x.left), right.map(x -> x.left));
        Optional<Sigma> sigma = mergeSigma(left.map(x -> x.right), right.map(x -> x.right));
        return merge.flatMap(x -> sigma.map(y -> new Pair<>(x, y)));  // nested `map` OK, result should be empty only when Opt is empty
    }

    public static Optional<Sigma> mergeSigma(
            Optional<Sigma> left, Optional<Sigma> right) {
        return left.flatMap(x ->
                right.flatMap(y ->
                        x.equals(y) ? Optional.of(x) : Optional.empty()));  // nested `flatMap`, result may be empty even if Opt not empty
    }

    // !!! TODO refactor with GTLType.merge
    public static Optional<? extends GTLType> mergeSyntacticDeps(
            Optional<? extends GTLType> left, Optional<? extends GTLType> right) {
        /*if (left.isEmpty() || right.isEmpty()) {
            return Optional.empty();
        }
        GTLType l = left.get();
        GTLType r = right.get();
        if (l.equals(r)) {  // !!! TODO
            return left;
        } else {
            throw new RuntimeException("TODO");
        }*/
        return left.flatMap(x -> right.flatMap(x::merge));
    }


    /* ... */

    @Override
    public Map<Role, Set<Op>> getCommittingAux(int c, Set<Role> com) {
        if (com.contains(this.src)) {
            Set<Role> tmp = new HashSet<>(com);
            tmp.add(this.dst);
            Map<Role, Set<Op>> res = new HashMap<>();
            this.cases.values().stream().map(x -> x.getCommittingAux(c, tmp)).forEach(x -> {
                for (Map.Entry<Role, Set<Op>> y : x.entrySet()) {
                    Set<Op> bar = res.computeIfAbsent(y.getKey(), z -> new HashSet<>());
                    bar.addAll(y.getValue());
                }
            });
            Set<Op> ops = res.computeIfAbsent(this.dst, x -> new HashSet<>());
            ops.addAll(this.cases.keySet());
            return res;
        } else {
            Map<Role, Set<Op>> res = new HashMap<>();
            this.cases.values().stream().map(x -> x.getCommittingAux(c, com)).forEach(x -> {
                for (Map.Entry<Role, Set<Op>> y : x.entrySet()) {
                    Set<Op> bar = res.computeIfAbsent(y.getKey(), z -> new HashSet<>());
                    bar.addAll(y.getValue());
                }
            });
            return res;
        }
    }


    /* Aux */

    @Override
    public GTGInteraction subs(RecVar v, GTGRecursion subs) {
        LinkedHashMap<Op, GTGType> cases = this.cases.entrySet().stream()
                                                     .collect(Collectors.toMap(
                                                             Map.Entry::getKey,
                                                             x -> x.getValue().subs(v, subs),
                                                             (x, y) -> null,
                                                             LinkedHashMap::new
                                                     ));
        return new GTGInteraction(this.src, this.dst, new LinkedHashMap<>(this.pays), cases);
    }

    @Override
    public String toString() {
        return this.src + "->" + this.dst
                + "{" + this.cases.entrySet().stream()
                                  .map(e -> msgToString(e.getKey()) + "." + e.getValue())
                                  .collect(Collectors.joining(", ")) + "}";
    }

    protected String msgToString(Op op) {
        //return op + (!this.pays.containsKey(op) ? "" : "(" + this.pays.get(op) + ")");
        return msgToString(op, this.pays.get(op));
    }

    public static String msgToString(Op op, Payload pay) {
        return op.toString() + pay;
    }


    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTGType.GLOBAL_CHOICE_HASH;
        hash = 31 * hash + this.src.hashCode();
        hash = 31 * hash + this.dst.hashCode();
        hash = 31 * hash + this.pays.hashCode();
        hash = 31 * hash + this.cases.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) { return true; }
        if (obj == null || !(obj instanceof GTGInteraction)) { return false; }
        GTGInteraction them = (GTGInteraction) obj;
        return them.canEquals(this)
                && this.src.equals(them.src)
                && this.dst.equals(them.dst)
                && this.pays.equals(them.pays)
                && this.cases.equals(them.cases);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTGInteraction;
    }





















}
