package org.scribble.ext.ea.core.config;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.core.type.session.SigLit;
import org.scribble.core.type.session.local.LRecv;
import org.scribble.core.type.session.local.LSend;
import org.scribble.core.type.session.local.LTypeFactory;
import org.scribble.ext.ea.core.process.*;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.Delta;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.util.EAPPair;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

// cf. T-Session and (nested) T-Par (missing)
// CHECKME: equiv to normal form with all \nu s at top?  sufficiently general?
public class EAPSystem {

    @NotNull public final Delta annots;
    @NotNull public final LinkedHashMap<EAPPid, EAPConfig> configs;

    @NotNull protected final LTypeFactory lf;

    public EAPSystem(@NotNull LTypeFactory lf,
                     @NotNull Delta annots,
                     @NotNull LinkedHashMap<EAPPid, EAPConfig> configs) {
        this.lf = lf;
        this.annots = annots;
        this.configs = configs.entrySet().stream().collect(
                Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new));
    }

    // !!! TODO safety
    //public void type(Gamma gamma, Delta delta, Delta delta1) {
    public void type(Gamma gamma, Delta delta) {
        for (EAPConfig c : this.configs.values()) {
            LinkedHashSet<Pair<EAPSid, Role>> eps = c.getEndpoints();
            LinkedHashMap<Pair<EAPSid, Role>, EALType> tmp = new LinkedHashMap<>(delta.map);
            for (Pair<EAPSid, Role> p : eps) {
                if (!this.annots.map.containsKey(p)) {
                    throw new RuntimeException("Unknown endpoint: " + p);
                }
                // !!! TODO: Delta disjoint union op
                tmp.put(p, this.annots.map.get(p));  // !!! splits outer Delta (not an actual name restriction) -- cf. cf. T-Session introduce Delta', T-Par split Delta
            }
            c.type(gamma, new Delta(tmp));
        }
    }

    /*@Deprecated  // For now
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
        }* /
        return collect;
    }*/

    public Map<EAPPid, Set<EAPPid>> canStep() {
        return this.configs.entrySet().stream()
                .filter(x -> x.getValue().canStep(this).left)
                .collect(Collectors.toMap(
                        Map.Entry::getKey,
                        x -> x.getValue().canStep(this).right
                ));
    }

    // Pre: p \in getReady ?
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
        if (!(foo instanceof EAPSuspend || foo instanceof EAPReturn
                || foo instanceof EAPSend || foo instanceof EAPApp || foo instanceof EAPLet)) {
            throw new RuntimeException("TODO: " + foo);
        }

        System.out.println("\naaa: " + p + " ,, " + foo.getClass() + " ,, " + foo);

        if (foo instanceof EAPSuspend || foo instanceof EAPReturn
                || foo instanceof EAPApp || foo instanceof EAPLet) {  // TODO refactor EAPSend to config step also
            LinkedHashMap<EAPPid, EAPConfig> configs = c.step(this);
            return new EAPSystem(this.lf, this.annots, configs);
        }

        // HERE HERE -- refactor remaining EAPSend reduction to EAPConfig -> refactor config step to separate case by case actions
        // for p: config.step(sys) -> Map<EAPPid, EAPConfig> -- all updated configs, including p's
        // ...maybe take `qs` for partner configs as param here -- cf. EAPConfig.canStep Set<Pid>
        // step annots -- only for EAPSend

        //EAPSystem res = new EAPSystem(this.configs);
        LinkedHashMap<EAPPid, EAPConfig> configs = new LinkedHashMap<>(this.configs);
        LinkedHashMap<Pair<EAPSid, Role>, EALType> dmap = new LinkedHashMap<>(this.annots.map);

        EAPThreadState t1;
        if (foo instanceof EAPSend) {
            //t1 = EAPRuntimeFactory.factory.activeThread(t.expr.recon(foo, EAPFactory.factory.returnn(EAPFactory.factory.unit())), t.sid, t.role);
            t1 = EAPRuntimeFactory.factory.activeThread(t.expr.foo(), t.sid, t.role);
            EAPSend cast = (EAPSend) foo;

            Optional<Map.Entry<EAPPid, EAPConfig>> fst =
                    this.configs.entrySet().stream().filter(x ->
                            x.getValue().sigma.keySet().stream().anyMatch(y ->
                                    y.left.equals(t.sid) && y.right.equals(cast.dst))
                    ).findFirst();
            if (fst.isEmpty()) {
                throw new RuntimeException("FIXME");  // !!! XXX HERE EAPExpr.getFoo gets "potenitally reducible parts", such as sends -- but receive may not be ready yet (e.g., handler not installed yet)
                // Currently PRE: `p` must have reducible step (i.e., send must have matching receive ready)
            }
            Map.Entry<EAPPid, EAPConfig> get = fst.get();
            EAPPid p2 = get.getKey();
            EAPConfig c2 = get.getValue();
            Map<Pair<EAPSid, Role>, EAPHandlers> sigma2 = c2.sigma;
            Pair<EAPSid, Role> k2 = new EAPPair<>(t.sid, cast.dst);
            EAPHandler vh = sigma2.get(k2).Hs.get(cast.op);  // non-null by pre?
            EAPExpr e2 = vh.expr.subs(Map.of(vh.var, cast.val));
            LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> newsigma2 =
                    new LinkedHashMap<>(c2.sigma);
            newsigma2.remove(k2);
            EAPActiveThread newt2 = EAPRuntimeFactory.factory.activeThread(e2, t.sid, k2.right);
            //res.configs.put(p2, EAPRuntimeFactory.factory.config(c2.pid, newt2, newsigma2));
            configs.put(p2, EAPRuntimeFactory.factory.config(c2.pid, newt2, newsigma2));

            EALType l2 = this.annots.map.get(k2);
            LRecv lr = this.lf.LRecv(null, t.role, new SigLit(cast.op, Payload.EMPTY_PAYLOAD));  // from foo  // FIXME EMPTY
            Optional<EALType> opt2 = l2.step(lr);
            if (!opt2.isPresent()) {
                throw new RuntimeException("TODO");
            }
            l2 = opt2.get();
            dmap.put(k2, l2);

            EAPPair k1 = new EAPPair(t.sid, t.role);
            EALType l1 = this.annots.map.get(k1);
            LSend ls = this.lf.LSend(null, new SigLit(cast.op, Payload.EMPTY_PAYLOAD), cast.dst);  // from foo  // FIXME EMPTY
            Optional<EALType> opt1 = l1.step(ls);
            if (!opt1.isPresent()) {
                throw new RuntimeException("TODO");
            }
            l1 = opt1.get();
            dmap.put(k1, l1);

        } else {
            throw new RuntimeException("TODO: " + foo);
        }

        LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigma1 = new LinkedHashMap<>(c.sigma);

        EAPConfig c1 = EAPRuntimeFactory.factory.config(c.pid, t1, sigma1);
        //res.configs.put(p, c1);
        configs.put(p, c1);

        Delta d1 = new Delta(dmap);

        return new EAPSystem(this.lf, d1, configs);
    }

    public Map<EAPPid, EAPConfig> getConfigs() {
        return Collections.unmodifiableMap(this.configs);
    }

    @Override
    public String toString() {
        return "[annots =" + this.annots.map + "\n configs="
                + this.configs.entrySet().stream().map(x -> x.toString()).collect(Collectors.joining("\n"))
                + "]";
    }
}
