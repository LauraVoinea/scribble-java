package org.scribble.ext.ea.core.config;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.process.*;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.Delta;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.util.EAPPair;
import org.scribble.ext.ea.util.EATriple;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

// cf. T-Session and (nested) T-Par (missing)
// CHECKME: equiv to normal form with all \nu s at top?  sufficiently general?
public class EAPSystem {

    //protected

    @NotNull public final LinkedHashMap<EAPPid, EAPConfig> configs;

    public EAPSystem(@NotNull LinkedHashMap<EAPPid, EAPConfig> configs) {
        this.configs = configs.entrySet().stream().collect(
                Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new));
    }

    // !!! TODO safety
    public void type(Gamma gamma, Delta delta, Delta delta1) {
        for (EAPConfig c : this.configs.values()) {
            LinkedHashSet<Pair<EAPSid, Role>> eps = c.getEndpoints();
            LinkedHashMap<Pair<EAPSid, Role>, EALType> tmp = new LinkedHashMap<>(delta.map);
            for (Pair<EAPSid, Role> p : eps) {
                if (!delta1.map.containsKey(p)) {
                    throw new RuntimeException("Unknown endpoint: " + p);
                }
                // !!! TODO: Delta disjoint union op
                tmp.put(p, delta1.map.get(p));  // !!! splits outer Delta (not an actual name restriction) -- cf. cf. T-Session introduce Delta', T-Par split Delta
            }
            c.type(gamma, new Delta(tmp));
        }
    }

    @Deprecated  // For now
    public Set<Pair<EAPPid, EAPExpr>> getReady() {
        // Includes sends, but not matching receives
        Set<Pair<EAPPid, EAPExpr>> collect = this.configs.entrySet().stream().filter(x -> x.getValue().isActive())
                .map(x -> new EAPPair<>(x.getKey(), ((EAPActiveThread) x.getValue().T).expr.getFoo()))
                .filter(x -> !(x.right instanceof EAPSend) || !this.configs.get(((EAPSend) x.right).dst).isActive())  // dst is idle...
                .collect(Collectors.toSet());
        /*Set<Pair<Role, EAPExpr>> res = new HashSet<>();
        for (Pair<Role, EAPExpr> p : collect) {
            if (p.right instanceof EAPSend) {
                EAPSend cast = (EAPSend) p.right;
                if (!this.configs.get(cast.dst).isActive()) {  // Idle...
                    res.add(p);
                }
            } else {
                res.add(p);
            }
        }*/
        return collect;
    }

    public EAPSystem reduce(EAPPid p) {  // n.b. beta is deterministic
        EAPConfig c = this.configs.get(p);
        if (!c.isActive()) {
            throw new RuntimeException("Stuck: " + p + " " + c);
        }
        EAPActiveThread t = (EAPActiveThread) c.T;
        if (!t.expr.isGround()) {
            throw new RuntimeException("Stuck: " + p + " " + c);
        }
        EAPExpr foo = t.expr.getFoo();
        if (!(foo instanceof EAPSuspend || foo instanceof EAPReturn || foo instanceof EAPSend)) {
            throw new RuntimeException("TODO: " + foo);
        }

        //EAPSystem res = new EAPSystem(this.configs);
        LinkedHashMap<EAPPid, EAPConfig> configs = new LinkedHashMap<>(this.configs);

        LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigma1 = new LinkedHashMap<>(c.sigma);
        if (foo instanceof EAPSuspend) {
            EAPSuspend cast = (EAPSuspend) foo;
            sigma1.put(new EAPPair<>(t.sid, t.role), (EAPHandlers) cast.val);  // t.role = r
        } else if (foo instanceof EAPSend || foo instanceof EAPReturn) {
            // skip
        } else {
            throw new RuntimeException("TODO: " + foo);
        }

        EAPThreadState t1;
        if (foo instanceof EAPSend) {
            //t1 = EAPRuntimeFactory.factory.activeThread(t.expr.recon(foo, EAPFactory.factory.returnn(EAPFactory.factory.unit())), t.sid, t.role);
            t1 = EAPRuntimeFactory.factory.activeThread(t.expr.beta(), t.sid, t.role);
            EAPSend cast = (EAPSend) foo;

            Optional<Map.Entry<EAPPid, EAPConfig>> fst =
                    this.configs.entrySet().stream().filter(x ->
                            x.getValue().sigma.keySet().stream().anyMatch(y ->
                                    y.left.equals(t.sid) && y.right.equals(cast.dst))
                    ).findFirst();
            if (fst.isEmpty()) {
                throw new RuntimeException("FIXME");  // EAPExpr.getFoo broken
            }
            Map.Entry<EAPPid, EAPConfig> get = fst.get();
            EAPPid p2 = get.getKey();
            EAPConfig c2 = get.getValue();
            Map<Pair<EAPSid, Role>, EAPHandlers> sigma2 = c2.sigma;
            Pair<EAPSid, Role> k2 = new EAPPair<>(t.sid, cast.dst);
            EATriple<EAPVar, EAValType, EAPExpr> vh =
                    sigma2.get(k2).Hs.get(cast.op);
            EAPExpr e2 = vh.right.subs(Map.of(vh.left, cast.val));
            LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> newsigma2 =
                    new LinkedHashMap<>(c2.sigma);
            newsigma2.remove(k2);
            EAPActiveThread newt2 = EAPRuntimeFactory.factory.activeThread(e2, t.sid, k2.right);
            //res.configs.put(p2, EAPRuntimeFactory.factory.config(c2.pid, newt2, newsigma2));
            configs.put(p2, EAPRuntimeFactory.factory.config(c2.pid, newt2, newsigma2));
        } else if (foo instanceof EAPSuspend || foo instanceof EAPReturn) {
            if (t.expr.equals(foo)) {  // top level
                t1 = EAPIdle.IDLE;
            } else {
                t1 = EAPRuntimeFactory.factory.activeThread(t.expr.beta(), t.sid, t.role);
            }
        } else {
            throw new RuntimeException("TODO: " + foo);
        }

        EAPConfig c1 = EAPRuntimeFactory.factory.config(c.pid, t1, sigma1);
        //res.configs.put(p, c1);
        configs.put(p, c1);
        return new EAPSystem(configs);
    }

    public Map<EAPPid, EAPConfig> getConfigs() {
        return Collections.unmodifiableMap(this.configs);
    }

    @Override
    public String toString() {
        return this.configs.toString();
    }
}
