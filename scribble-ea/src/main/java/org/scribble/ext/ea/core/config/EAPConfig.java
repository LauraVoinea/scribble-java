package org.scribble.ext.ea.core.config;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.process.*;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.Delta;
import org.scribble.ext.ea.core.type.session.local.EALEndType;
import org.scribble.ext.ea.core.type.session.local.EALInType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAUnitType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.util.EAPPair;
import org.scribble.ext.ea.util.EATriple;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

// cf. T-Actor
public class EAPConfig implements EAPRuntimeTerm {

    @NotNull
    public final EAPPid pid;
    @NotNull
    public final EAPThreadState T;
    @NotNull
    public final Map<Pair<EAPSid, Role>, EAPHandlers> sigma;  // !!! handlers specifically

    protected EAPConfig(@NotNull EAPPid pid,
                        @NotNull EAPThreadState T,
                        @NotNull LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> handlers) {
        this.pid = pid;
        this.T = T;
        this.sigma = Collections.unmodifiableMap(handlers.entrySet()
                .stream().collect(Collectors.toMap(
                        Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new)));
    }

    // Return set is (sync) "dependencies" needed to step if any
    public Pair<Boolean, Set<EAPPid>> canStep(EAPSystem sys) {
        if (!(this.T instanceof EAPActiveThread)) {
            return new Pair<>(false, Collections.emptySet());
        }
        return ((EAPActiveThread) this.T).canStep(sys);
    }

    // Deterministic w.r.t. "self" (cf. getFoo is singular) -- At least must be w.r.t. a given s for session safety -- could have multiple inbox msgs but currently installed handlers can only accept exactly one
    // Pre: getFoo + foo OK -- cf. EAPActiveThread.canStep -- TODO optimise away getFoo
    // cf. EAPActiveThread.canStep
    public LinkedHashMap<EAPPid, EAPConfig> step(EAPSystem sys) {
        if (!(this.T instanceof EAPActiveThread)) {
            throw new RuntimeException("Shouldn't get here: ");
        }
        EAPActiveThread t = (EAPActiveThread) this.T;
        EAPExpr foo = t.expr.getFoo();

        // TODO refactor separate case by case (rather than grouping thread/sigma/config creation)

        // top-level return ()
        if (foo instanceof EAPReturn) {
            if (t.expr.equals(foo)) {  // top level
                LinkedHashMap<EAPPid, EAPConfig> configs = new LinkedHashMap<>(sys.configs);
                LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigma1 = new LinkedHashMap<>(this.sigma);

                EAPThreadState t1 = EAPIdle.IDLE;  // XXX FIXME suspend V M should now go to M (not idle)
                EAPConfig c1 = EAPRuntimeFactory.factory.config(this.pid, t1, sigma1);
                //res.configs.put(p, c1);
                configs.put(this.pid, c1);
                return configs;
            } else {
                //t1 = EAPRuntimeFactory.factory.activeThread(t.expr.beta(), t.sid, t.role);
                throw new RuntimeException("Shouldn't get in here");
            }
        } else if (foo instanceof EAPSend) {
            throw new RuntimeException("TODO");
        }
        // Other non-beta cases
        else if (foo instanceof EAPSuspend) {
            if (t.expr.equals(foo)) {  // top level
                LinkedHashMap<EAPPid, EAPConfig> configs = new LinkedHashMap<>(sys.configs);
                LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigma1 = new LinkedHashMap<>(this.sigma);

                EAPThreadState t1 = EAPIdle.IDLE;  // XXX FIXME suspend V M should now go to M (not idle)
                EAPSuspend cast = (EAPSuspend) foo;
                sigma1.put(new EAPPair<>(t.sid, t.role), (EAPHandlers) cast.val);  // t.role = r
                EAPConfig c1 = EAPRuntimeFactory.factory.config(this.pid, t1, sigma1);
                configs.put(this.pid, c1);
                return configs;
            } else {
                //t1 = EAPRuntimeFactory.factory.activeThread(t.expr.beta(), t.sid, t.role);
                throw new RuntimeException("Shouldn't get in here");
            }
        }
        // LiftM beta cases
        else if (foo instanceof EAPApp || foo instanceof EAPLet) {
            LinkedHashMap<EAPPid, EAPConfig> configs = new LinkedHashMap<>(sys.configs);
            LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigma1 = new LinkedHashMap<>(this.sigma);

            EAPThreadState t1 = EAPRuntimeFactory.factory.activeThread(t.expr.foo(), t.sid, t.role);
            EAPConfig c1 = EAPRuntimeFactory.factory.config(this.pid, t1, sigma1);
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

    public LinkedHashSet<Pair<EAPSid, Role>> getEndpoints() {
        LinkedHashSet<Pair<EAPSid, Role>> res = new LinkedHashSet<>();
        if (!this.T.isIdle()) {
            EAPActiveThread t = (EAPActiveThread) this.T;
            res.add(new EAPPair<>(t.sid, t.role));
        }
        res.addAll(this.sigma.keySet());
        return res;
    }

    public boolean isActive() {
        return !this.T.isIdle();
    }

    public void type(Gamma gamma, Delta delta) {
        LinkedHashMap<Pair<EAPSid, Role>, EALType> tmp = new LinkedHashMap<>();
        if (this.T instanceof EAPActiveThread) { // !!! CHECKME
            EAPActiveThread at = (EAPActiveThread) this.T;
            Pair<EAPSid, Role> k = new EAPPair<>(at.sid, at.role);
            if (!delta.map.containsKey(k)) {
                throw new RuntimeException("Unknown endpoint: " + k + " ,, " + delta.map);
            }
            tmp.put(k, delta.map.get(k));
        }
        Delta delta1 = new Delta(tmp);
        this.T.type(gamma, delta1);  // !!! TODO split delta_1, delta_2 ?

        tmp = new LinkedHashMap<>(delta.map);
        if (this.T instanceof EAPActiveThread) { // !!! CHECKME
            tmp.remove(delta1.map.keySet().iterator().next());
        }
        Delta delta2 = new Delta(tmp);
        typeSigma(gamma, delta2);
    }

    // !!! TODO make sigma explicit (cf. TH-Handler)
    protected void typeSigma(Gamma gamma, Delta delta) {
        if (delta.map.size() != this.sigma.size()) {
            throw new RuntimeException("Invalid delta: " + delta + " |- " + this.sigma);
        }

        for (Map.Entry<Pair<EAPSid, Role>, EAPHandlers> e : this.sigma.entrySet()) {
            Pair<EAPSid, Role> k = e.getKey();
            if (!delta.map.containsKey(k)) {
                throw new RuntimeException("Unknown endpoint: " + k + " : " + delta.map);
            }
            EALType T = delta.map.get(k).unfoldAllOnce();  // !!! cf. EAPSystem this.annots.map.get(k2) -- use unfolded as annot -- XXX that only allows that many number of unfoldings during execution
            if (!(T instanceof EALInType)) {
                throw new RuntimeException("Invalid handler type: " + e + " :\n\t" + T);
            }
            EALInType cast = (EALInType) T;
            if (cast.peer.equals(k.right)) {
                throw new RuntimeException("Self communication not allowed: " + k + " ,, " + cast);
            }
            //EAPHandlers h = this.sigma.get(k);
            EAPHandlers h = e.getValue();
            if (!cast.peer.equals(h.role)) {
                throw new RuntimeException("Invalid handler type peer: " + e + " : " + T);
            }
            if (!cast.cases.keySet().equals(h.Hs.keySet())) {
                throw new RuntimeException("Bad handler set: " + cast.cases + " |> " + h.Hs);
            }
            // !!! TH-Handler typing the nested handler expr (uses Delta) -- cf. typing handler value TV-Handler (uses "infer")
            for (Map.Entry<Op, EAPHandler> x : h.Hs.entrySet()) {
                Op op = x.getKey();
                EAPHandler rhs = x.getValue();
                LinkedHashMap<EAName, EAValType> tmp = new LinkedHashMap<>(gamma.map);
                tmp.put(rhs.var, rhs.varType);
                Gamma gamma1 = new Gamma(tmp, new LinkedHashMap<>(gamma.fmap));
                Pair<EAValType, EALType> res = rhs.expr.type(gamma1, cast.cases.get(op).right);
                if (!res.equals(new EAPPair<>(EAUnitType.UNIT, EALEndType.END))) {
                    throw new RuntimeException("Badly typed: " + rhs.expr + " |> " + res);
                }
            }
        }
    }

    /* Aux */

    @Override
    public String toString() {
        return "<" + this.pid + ", " + this.T + ", " + this.sigma + ">";
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPConfig them = (EAPConfig) o;
        return them.canEquals(this)
                && this.pid.equals(them.pid)
                && this.T.equals(them.T)
                && this.sigma.equals(them.sigma);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPConfig;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.CONFIG;
        hash = 31 * hash + this.pid.hashCode();
        hash = 31 * hash + this.T.hashCode();
        hash = 31 * hash + this.sigma.hashCode();
        return hash;

    }
}
