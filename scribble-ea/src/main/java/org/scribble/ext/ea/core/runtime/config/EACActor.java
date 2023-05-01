package org.scribble.ext.ea.core.runtime.config;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.runtime.*;
import org.scribble.ext.ea.core.term.EAName;
import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.term.comp.*;
import org.scribble.ext.ea.core.term.expr.EAEHandlers;
import org.scribble.ext.ea.core.term.expr.EAExpr;
import org.scribble.ext.ea.core.term.expr.EAHandler;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.Delta;
import org.scribble.ext.ea.core.type.session.local.EALEndType;
import org.scribble.ext.ea.core.type.session.local.EALInType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

// cf. T-Actor
public class EACActor implements EAConfig {

    @NotNull public final EAPid pid;  // No longer in formal defs but useful in implementation
    @NotNull public final EAThread T;
    @NotNull public final Map<Pair<EASid, Role>, EAEHandlers> sigma;  // !!! handlers specifically

    @NotNull public EAExpr state;  // Pre: ground
    //public final Map<Pair<EAPSid, Role>, Integer> state;  // FIXME type // combine with sigma?

    public EACActor(@NotNull EAPid pid,
                    @NotNull EAThread T,
                    @NotNull LinkedHashMap<Pair<EASid, Role>, EAEHandlers> handlers,
                    //                @NotNull LinkedHashMap<Pair<EAPSid, Role>, Integer> state) {
                    @NotNull EAExpr state) {
        this.pid = pid;
        this.T = T;
        this.sigma = Collections.unmodifiableMap(handlers.entrySet()
                .stream().collect(Collectors.toMap(
                        Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new)));
        //this.state = Collections.unmodifiableMap(new LinkedHashMap<>(state));
        this.state = state;
    }

    // Return set is (sync) "dependencies" ("partner" pids) needed to step if any
    public Pair<Boolean, Set<EAPid>> canReduce(EASystem sys) {
        if (!(this.T instanceof EATActive)) {
            return new Pair<>(false, Collections.emptySet());
        }
        return ((EATActive) this.T).canActorReduce(sys);
    }

    // Deterministic w.r.t. "self" (cf. getFoo is singular) -- At least must be w.r.t. a given s for session safety -- could have multiple inbox msgs but currently installed handlers can only accept exactly one
    // Pre: getFoo + foo OK -- cf. EAPActiveThread.canStep -- TODO optimise away getFoo
    // cf. EAPActiveThread.canStep
    public LinkedHashMap<EAPid, EACActor> reduce(EASystem sys) {
        if (!(this.T instanceof EATActive)) {
            throw new RuntimeException("Shouldn't get here: ");
        }
        EATActive t = (EATActive) this.T;
        EAComp foo = t.expr.getConfigRedexCandidate();

        // TODO refactor separate case by case (rather than grouping thread/sigma/config creation)

        // top-level return ()
        if (foo instanceof EAMReturn) {
            if (t.expr.equals(foo)) {  // top level -- not really necessary to compare t.expr and foo, but checks foo working
                LinkedHashMap<EAPid, EACActor> configs = new LinkedHashMap<>(sys.actors);
                LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigma1 = new LinkedHashMap<>(this.sigma);
                EAMReturn cast = (EAMReturn) t.expr;

                EAThread t1 = EATIdle.IDLE;  // XXX FIXME suspend V M should now go to M (not idle)
                //EAPConfig c1 = EAPRuntimeFactory.factory.config(this.pid, t1, sigma1, new LinkedHashMap<>(this.state));
                //EAPConfig c1 = EARuntimeFactory.factory.config(this.pid, t1, sigma1, this.state);
                EACActor c1 = EARuntimeFactory.factory.config(this.pid, t1, sigma1, cast.val);
                //res.configs.put(p, c1);
                configs.put(this.pid, c1);
                return configs;
            } else {
                ////t1 = EAPRuntimeFactory.factory.activeThread(t.expr.beta(), t.sid, t.role);
                //throw new RuntimeException("Shouldn't get in here");

                // TODO factor out with other LiftM beta cases
                LinkedHashMap<EAPid, EACActor> configs = new LinkedHashMap<>(sys.actors);
                LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigma1 = new LinkedHashMap<>(this.sigma);
                EAThread t1 = EARuntimeFactory.factory.activeThread(t.expr.configReduce(), t.sid, t.role);
                //EAPConfig c1 = EAPRuntimeFactory.factory.config(this.pid, t1, sigma1, new LinkedHashMap<>(this.state));
                EACActor c1 = EARuntimeFactory.factory.config(this.pid, t1, sigma1, this.state);
                configs.put(this.pid, c1);
                return configs;
            }
        } else if (foo instanceof EAMSend) {

            LinkedHashMap<EAPid, EACActor> configs = new LinkedHashMap<>(sys.actors);

            EAThread t1;
            //t1 = EAPRuntimeFactory.factory.activeThread(t.expr.recon(foo, EAPFactory.factory.returnn(EAPFactory.factory.unit())), t.sid, t.role);
            t1 = EARuntimeFactory.factory.activeThread(t.expr.configReduce(), t.sid, t.role);
            EAMSend cast = (EAMSend) foo;

            Optional<Map.Entry<EAPid, EACActor>> fst =
                    sys.actors.entrySet().stream().filter(x ->
                            x.getValue().sigma.keySet().stream().anyMatch(y ->
                                    y.left.equals(t.sid) && y.right.equals(cast.dst))
                    ).findFirst();
            if (fst.isEmpty()) {
                throw new RuntimeException("FIXME");  // !!! XXX HERE EAPExpr.getFoo gets "potenitally reducible parts", such as sends -- but receive may not be ready yet (e.g., handler not installed yet)
                // Currently PRE: `p` must have reducible step (i.e., send must have matching receive ready)
            }
            Map.Entry<EAPid, EACActor> get = fst.get();
            EAPid p2 = get.getKey();
            EACActor c2 = get.getValue();
            Map<Pair<EASid, Role>, EAEHandlers> sigma2 = c2.sigma;
            Pair<EASid, Role> k2 = new Pair<>(t.sid, cast.dst);
            EAHandler vh = sigma2.get(k2).Hs.get(cast.op);  // non-null by pre?

            //EAPExpr e2 = vh.expr.subs(Map.of(vh.var, cast.val, vh.svar, EAPFactory.factory.intt(c2.state.get(k2))));
            EAComp e2 = vh.expr.subs(Map.of(vh.var, cast.val, vh.svar, c2.state));

            LinkedHashMap<Pair<EASid, Role>, EAEHandlers> newsigma2 =
                    new LinkedHashMap<>(c2.sigma);
            newsigma2.remove(k2);
            EATActive newt2 = EARuntimeFactory.factory.activeThread(e2, t.sid, k2.right);
            //res.configs.put(p2, EAPRuntimeFactory.factory.config(c2.pid, newt2, newsigma2));
            //configs.put(p2, EAPRuntimeFactory.factory.config(c2.pid, newt2, newsigma2, new LinkedHashMap<>(c2.state)));
            configs.put(p2, EARuntimeFactory.factory.config(c2.pid, newt2, newsigma2, c2.state));

            LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigma1 = new LinkedHashMap<>(this.sigma);

            //EAPConfig c1 = EAPRuntimeFactory.factory.config(this.pid, t1, sigma1, new LinkedHashMap<>(this.state));
            EACActor c1 = EARuntimeFactory.factory.config(this.pid, t1, sigma1, this.state);
            //res.configs.put(p, c1);
            configs.put(this.pid, c1);

            return configs;

        }
        // Other non-beta cases
        else if (foo instanceof EAMSuspend) {
            if (t.expr.equals(foo)) {  // top level
                LinkedHashMap<EAPid, EACActor> configs = new LinkedHashMap<>(sys.actors);
                LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigma1 = new LinkedHashMap<>(this.sigma);

                EAThread t1 = EATIdle.IDLE;
                EAMSuspend cast = (EAMSuspend) foo;
                sigma1.put(new Pair<>(t.sid, t.role), (EAEHandlers) cast.val);  // t.role = r

                //LinkedHashMap<Pair<EAPSid, Role>, Integer> tmp = new LinkedHashMap<>(this.state);
                //tmp.put(new Pair<>(t.sid, t.role), ((EAPIntVal) cast.sval).val);  // !!! FIXME currently works because suspend expr must have val (which must have been subst by now, i.e., ground)

                //EAPConfig c1 = EAPRuntimeFactory.factory.config(this.pid, t1, sigma1, tmp);
                EACActor c1 = EARuntimeFactory.factory.config(this.pid, t1, sigma1, cast.sval);
                configs.put(this.pid, c1);
                return configs;
            } else {
                //t1 = EAPRuntimeFactory.factory.activeThread(t.expr.beta(), t.sid, t.role);
                throw new RuntimeException("Shouldn't get in here");
            }
        }
        // LiftM beta cases
        else if (foo instanceof EAMApp || foo instanceof EAMLet || foo instanceof EAMIf) {
            LinkedHashMap<EAPid, EACActor> configs = new LinkedHashMap<>(sys.actors);
            LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigma1 = new LinkedHashMap<>(this.sigma);

            EAThread t1 = EARuntimeFactory.factory.activeThread(t.expr.configReduce(), t.sid, t.role);
            //EAPConfig c1 = EAPRuntimeFactory.factory.config(this.pid, t1, sigma1, new LinkedHashMap<>(this.state));
            EACActor c1 = EARuntimeFactory.factory.config(this.pid, t1, sigma1, this.state);
            configs.put(this.pid, c1);
            return configs;
        } /*else if (foo instanceof EAPLet) {
            LinkedHashMap<EAPPid, EAPConfig> configs = new LinkedHashMap<>(sys.configs);
            LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigma1 = new LinkedHashMap<>(this.sigma);

            EAPThreadState t1 = EAPRuntimeFactory.factory.activeThread(t.expr.foo(), t.sid, t.role);
            EAPConfig c1 = EAPRuntimeFactory.factory.config(this.pid, t1, sigma1);
            configs.put(this.pid, c1);
            return configs;
        }*/ else {
            throw new RuntimeException("TODO: " + foo);
        }
    }

    public LinkedHashSet<Pair<EASid, Role>> getEndpoints() {
        LinkedHashSet<Pair<EASid, Role>> res = new LinkedHashSet<>();
        if (!this.T.isIdle()) {
            EATActive t = (EATActive) this.T;
            res.add(new Pair<>(t.sid, t.role));
        }
        res.addAll(this.sigma.keySet());
        return res;
    }

    public boolean isActive() {
        return !this.T.isIdle();
    }

    @Override
    public Either<Exception, Tree<String>> type(Gamma gamma, Delta delta) {
        EAVType infer = this.state.infer();
        GammaState gamma2 = new GammaState(new LinkedHashMap<>(gamma.map), new LinkedHashMap<>(gamma.fmap), infer);
        //Gamma gamma2 = gamma1;

        LinkedHashMap<Pair<EASid, Role>, EALType> tmp = new LinkedHashMap<>();
        if (this.T instanceof EATActive) { // !!! CHECKME
            EATActive at = (EATActive) this.T;
            Pair<EASid, Role> k = new Pair<>(at.sid, at.role);
            if (!delta.map.containsKey(k)) {
                //throw new RuntimeException("Unknown endpoint: " + k + " ,, " + delta.map);
                return Either.left(new Exception("Unknown endpoint: " + k + " ,, " + delta.map));
            }
            tmp.put(k, delta.map.get(k));
        }
        Delta delta1 = new Delta(tmp);
        Either<Exception, Tree<String>> t = this.T.type(gamma2, delta1);// !!! TODO split delta_1, delta_2 ?
        if (t.isLeft()) {
            return t;
        }

        tmp = new LinkedHashMap<>(delta.map);
        if (this.T instanceof EATActive) { // !!! CHECKME
            tmp.remove(delta1.map.keySet().iterator().next());
        }
        Delta delta2 = new Delta(tmp);
        Either<Exception, Tree<String>> u = typeSigma(gamma2, delta2);
        return u.mapRight(x -> new Tree(
                "[T-Actor] " + toJudgementString(gamma, delta),
                t.getRight().get(), x));

        /*EAValType stype = this.state.type(gamma);
        if (!stype.equals(gamma.svarType)) {
            throw new RuntimeException("Expected state type " + gamma.svarType + ", not: " + stype);
        }*/
    }

    /* ... */

    // Combines TH-Empty and TH-Handler
    // !!! TODO make Sigma explicit class (cf. TH-Handler)
    protected Either<Exception, Tree<String>> typeSigma(GammaState gamma, Delta delta) {
        if (delta.map.size() != this.sigma.size()) {
            //throw new RuntimeException("Invalid delta: " + delta + " |- " + this.sigma);
            return Either.left(new Exception("Invalid delta: " + delta + " |- " + this.sigma));
        }

        List<Tree<String>> ds = new LinkedList<>();
        for (Map.Entry<Pair<EASid, Role>, EAEHandlers> e : this.sigma.entrySet()) {
            Pair<EASid, Role> k = e.getKey();
            if (!delta.map.containsKey(k)) {
                //throw new RuntimeException("Unknown endpoint: " + k + " : " + delta.map);
                return Either.left(new Exception("Unknown endpoint: " + k + " : " + delta.map));
            }
            EALType T = delta.map.get(k).unfoldAllOnce();  // !!! cf. EAPSystem this.annots.map.get(k2) -- use unfolded as annot -- XXX that only allows that many number of unfoldings during execution
            if (!(T instanceof EALInType)) {
                //throw new RuntimeException("Invalid handler type: " + e + " :\n\t" + T);
                return Either.left(new Exception("Invalid handler type: " + e + " :\n\t" + T));
            }
            EALInType cast = (EALInType) T;
            if (cast.peer.equals(k.right)) {
                //throw new RuntimeException("Self communication not allowed: " + k + " ,, " + cast);
                return Either.left(new Exception("Self communication not allowed: " + k + " ,, " + cast));
            }
            //EAPHandlers h = this.sigma.get(k);
            EAEHandlers h = e.getValue();
            if (!cast.peer.equals(h.role)) {
                //throw new RuntimeException("Invalid handler type peer: " + e + " : " + T);
                return Either.left(new Exception("Invalid handler type peer: " + e + " : " + T));
            }
            if (!cast.cases.keySet().equals(h.Hs.keySet())) {
                //throw new RuntimeException("Bad handler set: " + cast.cases + " |> " + h.Hs);
                return Either.left(new Exception("Bad handler set: " + cast.cases + " |> " + h.Hs));
            }

            // !!! TH-Handler typing the nested handler expr (uses Delta) -- cf. typing handler value TV-Handler (uses "infer")
            for (Map.Entry<Op, EAHandler> x : h.Hs.entrySet()) {
                Op op = x.getKey();
                EAHandler rhs = x.getValue();
                LinkedHashMap<EAName, EAVType> tmp = new LinkedHashMap<>(gamma.gamma.map);
                tmp.put(rhs.var, rhs.varType);

                tmp.put(rhs.var, rhs.varType);
                tmp.put(rhs.svar, rhs.svarType);

                GammaState gamma1 = new GammaState(tmp, new LinkedHashMap<>(gamma.gamma.fmap), //gamma.svar,
                        gamma.svarType);
                //Pair<EAVType, EALType> res = rhs.expr.type(gamma1, cast.cases.get(op).right);
                Either<Exception, Pair<Pair<EAVType, EALType>, Tree<String>>> t =
                        rhs.expr.type(gamma1, cast.cases.get(op).right);
                if (t.isLeft()) {
                    return Either.left(t.getLeft().get());
                }
                Pair<Pair<EAVType, EALType>, Tree<String>> pp = t.getRight().get();
                ds.add(pp.right);
                Pair<EAVType, EALType> res = pp.left;
                ////if (!res.equals(new Pair<>(EAVUnitType.UNIT, EALEndType.END))) {
                ////if (!res.equals(new Pair<>(gamma.svarType, EALEndType.END))) {
                Optional<EAVType> u = EAVType.unify(res.left, gamma.svarType);
                if (!u.isPresent() || !u.get().equals(gamma.svarType) || !res.right.equals(EALEndType.END)) {
                    //throw new RuntimeException("Badly typed: " + rhs.expr + " |> " + res);
                    return Either.left(new RuntimeException("Badly typed: " + rhs.expr + " |> " + res));
                }
            }
        }
        return Either.right(new Tree<>("[TH-Handler] " + toSigmaJudgementString(gamma, delta), ds));
    }

    public String toSigmaJudgementString(GammaState gamma, Delta delta) {
        return gamma + "; " + delta + " \u22a2 " + this.sigma;
    }

    /* Aux */

    @Override
    public String toString() {
        return "<" + this.pid + ", " + this.T + ", "
                + this.sigma + ", " + this.state + ">";
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EACActor them = (EACActor) o;
        return them.canEquals(this)
                && this.pid.equals(them.pid)
                && this.T.equals(them.T)
                && this.sigma.equals(them.sigma)
                && this.state.equals(them.state);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EACActor;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.ACTOR;
        hash = 31 * hash + this.pid.hashCode();
        hash = 31 * hash + this.T.hashCode();
        hash = 31 * hash + this.sigma.hashCode();
        hash = 31 * hash + this.state.hashCode();
        return hash;

    }
}
