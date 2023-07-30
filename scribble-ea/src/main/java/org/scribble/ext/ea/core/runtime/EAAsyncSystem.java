package org.scribble.ext.ea.core.runtime;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.core.type.session.SigLit;
import org.scribble.core.type.session.local.LRecv;
import org.scribble.core.type.session.local.LSend;
import org.scribble.core.type.session.local.LTypeFactory;
import org.scribble.ext.ea.core.runtime.config.EACActor;
import org.scribble.ext.ea.core.term.comp.*;
import org.scribble.ext.ea.core.term.expr.EAEAPName;
import org.scribble.ext.ea.core.term.expr.EAEUnit;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.*;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.*;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

// "normal form" (..\nu iotas..) (..\nu APs..) (..\nu sesssions..) [ ..APs.. || ..configs.. || ..queues.. ] -- cf. Theorem Progress
// cf. T-Session and (nested) T-Par (missing)
// CHECKME: equiv to normal form with all \nu s at top?  sufficiently general?
public class EAAsyncSystem {

    @NotNull protected static final EARuntimeFactory RF = EARuntimeFactory.factory;

    protected static int pidCounter = 1;

    // FIXME cf. exhaustive state testing
    protected int nextSpawnCounter() {
        return this.pidCounter++;
    }

    protected static int iCounter = 1;
    protected static Map<EAIota, EAPid> iotas = EAUtil.mapOf();  // static or instance?

    protected static int sidCounter = 1;

    protected EASid newSid() {
        return RF.sid("_s" + sidCounter++);
    }

    // FIXME cf. exhaustive state testing
    protected EAIota newIota(EAPid p) {
        EAIota iota = RF.iota("\u03b9" + iCounter++);
        EAAsyncSystem.iotas.put(iota, p);
        return iota;
    }

    // !!! TODO also unify sess names, spawn pids, (etc)?  currently only doing iotas
    public static boolean unifyIotas(EAAsyncSystem s1, EAAsyncSystem s2) {

        // Just enough checks for second part  // TODO add more checks to optimise early pruning
        if (!s1.actors.keySet().equals(s2.actors.keySet())) {
            return false;
        }
        if (!s1.access.keySet().equals(s2.access.keySet()) ||
                s1.access.entrySet().stream().anyMatch(x -> {
                    EAEAPName k_x = x.getKey();
                    Map<Role, Pair<EALType, List<EAIota>>> v1 = x.getValue();
                    Map<Role, Pair<EALType, List<EAIota>>> v2 = s2.access.get(k_x);
                    if (!v1.keySet().equals(v2.keySet())) {
                        return true;
                    }
                    if (v1.keySet().stream().anyMatch(r -> {
                        Pair<EALType, List<EAIota>> get1 = v1.get(r);
                        Pair<EALType, List<EAIota>> get2 = v2.get(r);
                        return !get1.left.equals(get2.left)  // Not needed to check here (cf. other actor details)
                                || get1.right.size() != get2.right.size();
                    })) {
                        return true;
                    }
                    return false;
                })
        ) {
            return false;
        }

        // ...

        // access2
        LinkedHashMap<EAEAPName, Map<Role, Pair<EALType, List<EAIota>>>> access2 = EAUtil.copyOfMap(s2.access);
        Map<EAIota, EAIota> subs = EAUtil.mapOf();
        for (Map.Entry<EAEAPName, Map<Role, Pair<EALType, List<EAIota>>>> x : s1.access.entrySet()) {
            EAEAPName ap = x.getKey();
            Map<Role, Pair<EALType, List<EAIota>>> v1 = x.getValue();
            Map<Role, Pair<EALType, List<EAIota>>> v2 = access2.get(ap);

            for (Role r : s2.access.get(ap).keySet()) {
                Pair<EALType, List<EAIota>> tmp2 = v2.get(r);
                List<EAIota> old = tmp2.right;
                List<EAIota> neww = v1.get(r).right;
                v2.put(r, Pair.of(tmp2.left, neww));
                Iterator<EAIota> it = neww.iterator();
                for (EAIota y : old) {
                    if (subs.containsKey(y)) {  // TODO ? "global" mgu based on (List<EAIota>, List<EAIota>) across every role in every AP
                        throw new RuntimeException("Shouldn't get here: " + y);
                    }
                    subs.put(y, it.next());
                }
            }
        }

        // rho2
        LinkedHashMap<EAPid, EACActor> actors2 = EAUtil.copyOf(s2.actors);
        for (EAPid pid : actors2.keySet()) {
            EACActor p = actors2.get(pid);
            Map<EAIota, EAComp> old = p.rho;
            LinkedHashMap<EAIota, EAComp> rho2 = EAUtil.mapOf();
            for (EAIota i : old.keySet()) {
                rho2.put(subs.get(i), old.get(i));
            }
            actors2.put(pid, RF.actor(p.pid, p.T, EAUtil.copyOf(p.sigma), rho2, p.state));
        }

        return s1.equals(RF.asyncSystem(s2.lf, actors2, EAUtil.copyOf(s2.queues), access2, s2.adelta));
    }

    /* ... */

    @NotNull protected final LTypeFactory lf;

    //@NotNull public final Delta annots;

    //@NotNull public final ...Global...  // project, chec against original annots (implies safety), and type check actors
    @NotNull public final AsyncDelta adelta;

    // Unmod LinkedHashMaps
    @NotNull public final Map<EAPid, EACActor> actors;  // keyset is all pids in system  // pids no longer in formal defs but useful in implementation
    @NotNull public final Map<EASid, EAGlobalQueue> queues;  // keyset is all sids in system

    @NotNull public final Map<EAEAPName, Map<Role, Pair<EALType, List<EAIota>>>> access;  // All roles must be provided

    public EAAsyncSystem(
            LTypeFactory lf,
            //Delta annots,
            LinkedHashMap<EAPid, EACActor> actors,
            LinkedHashMap<EASid, EAGlobalQueue> queues,
            LinkedHashMap<EAEAPName, Map<Role, Pair<EALType, List<EAIota>>>> access,
            AsyncDelta adelta) {

        if (actors.entrySet().stream().anyMatch(x -> !x.getKey().equals(x.getValue().pid))) {
            throw new RuntimeException("Invalid pid/config mapping: " + actors);
        }
        if (queues.entrySet().stream().anyMatch(x -> !x.getKey().equals(x.getValue().sid))) {
            throw new RuntimeException("Invalid sid/queue mapping: " + queues);
        }
        this.lf = lf;
        //this.annots = annots;
        this.actors = EAUtil.umodCopyOf(actors);
        this.queues = EAUtil.umodCopyOf(queues);
        this.access = EAUtil.umodCopyOfMap(access);

        this.adelta = adelta;
    }

    /* ... */

    // !!! TODO "safety property" -- global+projection
    public Either<Exception, List<Tree<String>>> type() {  // TODO refactor
        return type(new Gamma());
    }

    public Either<Exception, List<Tree<String>>> type(Gamma gamma) {
        List<Tree<String>> res = new LinkedList<>();
        //Set<Pair<EASid, Role>> todo = new HashSet<>(this.annots.map.keySet());
        Set<Pair<EASid, Role>> todo = new HashSet<>(this.adelta.types.keySet());
        for (EACActor c : this.actors.values()) {
            LinkedHashSet<Pair<EASid, Role>> eps = c.getEndpoints();
            LinkedHashMap<Pair<EASid, Role>, EALType> tmp = new LinkedHashMap<>();
            for (Pair<EASid, Role> ep : eps) {
                if (!todo.contains(ep)) {
                    return Either.left(new Exception("Unknown endpoint or already used: " + ep));
                }
                todo.remove(ep);
                // !!! TODO: Delta disjoint union op
                //tmp.put(p, this.annots.map.get(p));  // !!! splits outer Delta (not an actual name restriction) -- cf. cf. T-Session introduce Delta', T-Par split Delta
                tmp.put(ep, this.adelta.types.get(ep));  // !!! splits outer Delta (not an actual name restriction) -- cf. cf. T-Session introduce Delta', T-Par split Delta
            }
            Either<Exception, Tree<String>> t = c.type(gamma, new Delta(tmp));
            if (t.isLeft()) {
                return Either.left(t.getLeft());
            }
            res.add(t.getRight());
        }
        //if (todo.stream().anyMatch(x -> !this.annots.map.get(x).equals(EALEndType.END))) {
        if (todo.stream().anyMatch(x -> !this.adelta.types.get(x).equals(EALEndType.END))) {
            return Either.left(new Exception("Endpoints not implemented: "
                    //+ todo + "\n\tannots = " + this.annots));
                    + todo + "\n\tdelta = " + this.adelta));
        }
        return Either.right(res);
    }

    /* ... */

    public Pair<
            Map<EAPid, Either<Map<EASid, Either<EAComp, EAMsg>>, Set<EAComp>>>,
            Set<Pair<EAEAPName, Map<EAPid, Pair<Role, EAIota>>>>>
    getSteppable() {
        Map<EAPid, Map<EASid, Either<EAComp, EAMsg>>> res = EAUtil.mapOf();
        Map<EAPid, Set<EAComp>> res2 = EAUtil.mapOf();  // TODO refactor

        for (Map.Entry<EAPid, EACActor> ep : this.actors.entrySet()) {
            EAPid pid = ep.getKey();
            EACActor C = ep.getValue();
            EAThreadMode mode = C.T.getMode();  // TODO refactor

            if (mode == EAThreadMode.IDLE || mode == EAThreadMode.SESSION) {
                for (Map.Entry<EASid, EAGlobalQueue> es : this.queues.entrySet()) {
                    EASid sid = es.getKey();
                    EAGlobalQueue queue = es.getValue();
                    Optional<Either<EAComp, EAMsg>> opt = C.getStepSubexprsE(queue);  // TODO refactor -- actor should return session cands Map<EASid, Either<EAComp, EAMsg>> and non-sess cands
                    if (opt.isPresent()) {
                        Either<EAComp, EAMsg> e = opt.get();
                        Map<EASid, Either<EAComp, EAMsg>> tmp = res.computeIfAbsent(pid, x -> EAUtil.mapOf());
                        if (e.isLeft()) {
                            tmp.put(sid, Either.left(e.getLeft()));
                        } else {
                            tmp.put(sid, Either.right(e.getRight()));
                        }
                    }
                }

            } else {  // mode == EAThreadMode.NO_SESSION
                Set<EAComp> get = C.getStepSubexprsE0();
                if (res2.containsKey(pid)) {
                    throw new RuntimeException("Shouldn't get here: " + pid);
                }
                res2.put(pid, get);
            }
        }

        // [E-Init]
        Set<Pair<EAEAPName, Map<EAPid, Pair<Role, EAIota>>>> res3 = EAUtil.setOf();
        for (Map.Entry<EAEAPName, Map<Role, Pair<EALType, List<EAIota>>>> x : this.access.entrySet()) {
            EAEAPName ap = x.getKey();  // map val not used
            Optional<Map<Role, EAIota>> init = getInit(ap, new HashMap<>(), new HashSet<>());
            if (!init.isPresent()) {
                continue;
            }
            Map<Role, EAIota> get = init.get();
            res3.add(Pair.of(
                    ap,
                    get.entrySet().stream().collect(
                            Collectors.toMap(
                                    y -> this.iotas.get(y.getValue()),
                                    y -> Pair.of(y.getKey(), y.getValue())
                            ))
            ));
        }

        Map<EAPid, Either<Map<EASid, Either<EAComp, EAMsg>>, Set<EAComp>>> tmp = EAUtil.mapOf();
        for (EAPid p : res.keySet()) {
            tmp.put(p, Either.left(res.get(p)));
        }
        for (EAPid p : res2.keySet()) {
            if (tmp.containsKey(p)) {
                throw new RuntimeException("Shouldn't get here: " + p);
            }
            tmp.put(p, Either.right(res2.get(p)));
        }
        return Pair.of(tmp, res3);
    }

    private Optional<Map<Role, EAIota>> getInit(EAEAPName ap, Map<Role, EAIota> curr, Set<EAPid> ps) {
        Map<Role, Pair<EALType, List<EAIota>>> iotas = this.access.get(ap);
        Set<Role> rs = new HashSet<>(iotas.keySet());
        rs.removeAll(curr.keySet());
        if (rs.isEmpty()) {
            return Optional.of(curr);
        }
        Role r = rs.iterator().next();
        List<EAIota> is = iotas.get(r).right;
        for (EAIota i : is) {
            EAPid p = this.iotas.get(i);
            if (this.actors.get(p).T.getMode() != EAThreadMode.IDLE || ps.contains(p)) {
                continue;
            }
            Map<Role, EAIota> curr1 = new HashMap<>(curr);
            curr1.put(r, i);
            Set<EAPid> ps1 = new HashSet<>(ps);
            ps1.add(p);
            Optional<Map<Role, EAIota>> res = getInit(ap, curr1, ps1);
            if (res.isPresent()) {
                return res;
            }
        }
        return Optional.empty();
    }

    // [E-Send], [E-Suspend], [E-Reset], [E-Spawn], [E-Register], [E-Lift] -- cf. react for [E-React]
    public Either<Exception, Triple<EAAsyncSystem, Tree<String>, Tree<String>>> step(
            EAPid p, EASid s, EAComp e) {  // s only used for [E-Send], factor out?  // n.b. beta is deterministic

        // Based on candidate fragment e, do stepAsync0/1 on subj actor depending if queue also needed
        //    - actor will step the comp of its session/no-session thread by e -- basic stepping is same for both E/M-context
        // Also potentially update other config/queue depending on candidate fragment

        EACActor c = this.actors.get(p); // p.equals(c.pid)

        // Session typing (annots) and AP state unchanged -- s not used
        if (e instanceof EAMSuspend  // [E-Suspend]
                || e instanceof EAMReturn  // [E-Reset]
                || e instanceof EAMApp || e instanceof EAMLet || e instanceof EAMIf  // [E-Lift]
                || e instanceof EAMBinOp)  // also [E-Lift]
        {
            Either<Exception, Pair<EACActor, Tree<String>>> step = c.stepAsync0(e);
            return step.mapRight(x -> {
                LinkedHashMap<EAPid, EACActor> actors1 = EAUtil.copyOf(this.actors);
                actors1.put(p, x.left);
                LinkedHashMap<EASid, EAGlobalQueue> queues = EAUtil.copyOf(this.queues);
                return Triple.of(
                        RF.asyncSystem(this.lf, actors1, queues, EAUtil.copyOf(this.access), this.adelta),
                        x.right,
                        null  // XXX TODO -- or fine when no delta deriv?
                );
            });
        }

        // Actor/AP state changed
        else if (e instanceof EAMSpawn) {
            Either<Exception, Pair<EACActor, Tree<String>>> step = c.stepAsync0(e);
            if (step.isLeft()) {
                return Either.left(step.getLeft());
            }
            Pair<EACActor, Tree<String>> get = step.getRight();

            LinkedHashMap<EAPid, EACActor> actors1 = EAUtil.copyOf(this.actors);
            actors1.put(p, get.left);

            EAMSpawn cast = (EAMSpawn) e;
            EAPid spawn = RF.pid(p.id + "_" + nextSpawnCounter());
            actors1.put(spawn, RF.actor(spawn, RF.noSessionThread(cast.M), EAUtil.mapOf(), EAUtil.mapOf(), //null));  // FIXME HACK state
                    EAEUnit.UNIT));

            return Either.right(Triple.of(
                    RF.asyncSystem(this.lf, actors1, EAUtil.copyOf(this.queues), EAUtil.copyOf(this.access),
                            this.adelta),
                    get.right,
                    null  // XXX TODO -- or fine when no delta deriv?
            ));

        } else if (e instanceof EAMRegister) {
            EAIota iota = newIota(p);
            Either<Exception, Pair<EACActor, Tree<String>>> step = c.stepAsync2(e, iota);
            if (step.isLeft()) {
                return Either.left(step.getLeft());
            }
            Pair<EACActor, Tree<String>> get = step.getRight();

            LinkedHashMap<EAPid, EACActor> actors1 = EAUtil.copyOf(this.actors);
            actors1.put(p, get.left);

            EAMRegister cast = (EAMRegister) e;
            if (!(cast.V instanceof EAEAPName)) {
                return Either.left(newStuck("Expected AP name, not " + cast.V + "in: " + cast));
            }
            EAEAPName ap = (EAEAPName) cast.V;
            LinkedHashMap<EAEAPName, Map<Role, Pair<EALType, List<EAIota>>>> access =
                    EAUtil.copyOf(this.access);
            if (!access.containsKey(ap)) {
                return Either.left(newStuck("Access point " + ap + " not found: " + this.access));
            }
            Map<Role, Pair<EALType, List<EAIota>>> invites = EAUtil.copyOf(access.get(ap));
            if (!invites.containsKey(cast.role)) {
                return Either.left(newStuck("Unknown role " + cast.role + ", expected: " + invites.keySet()));
            }
            /*List<EAIota> is = invites.containsKey(cast.role)  // XXX pre: all roles already present
                    ? EAUtil.copyOf(invites.get(cast.role))
                    : EAUtil.listOf();*/
            Pair<EALType, List<EAIota>> tmp = invites.get(cast.role);
            List<EAIota> is = EAUtil.copyOf(tmp.right);
            is.add(iota);
            invites.put(cast.role, Pair.of(tmp.left, is));
            access.put(ap, invites);

            return Either.right(Triple.of(
                    RF.asyncSystem(this.lf, actors1, EAUtil.copyOf(this.queues), access,
                            this.adelta),
                    get.right,
                    null  // XXX TODO -- or fine when no delta deriv?
            ));
        }

        // Session typing changed -- annots updated
        // [E-Send]
        else if (e instanceof EAMSend) {

            EATSession active = (EATSession) c.T;
            EAMSend cast = (EAMSend) e;
            EAGlobalQueue queue = queues.get(s);

            Either<Exception, Triple<EACActor, EAGlobalQueue, Tree<String>>> step =
                    c.stepAsync1(e, queue);
            if (step.isLeft()) {
                return Either.left(step.getLeft());
            }
            Triple<EACActor, EAGlobalQueue, Tree<String>> get = step.getRight();

            LinkedHashMap<EAPid, EACActor> actors1 = EAUtil.copyOf(this.actors);
            actors1.put(p, get.left);
            LinkedHashMap<EASid, EAGlobalQueue> queues1 = EAUtil.copyOf(this.queues);
            queues1.put(s, get.mid);

            LSend a = this.lf.LSend(null, new SigLit(cast.op, Payload.EMPTY_PAYLOAD), cast.dst);  // FIXME EMPTY_PAY

            /*LinkedHashMap<Pair<EASid, Role>, EALType> dmap = EAUtil.copyOf(this.annots.map);
            Pair<EASid, Role> k1 = new Pair<>(s, active.role);
            EALType l1 = this.annots.map.get(k1);
            Optional<EALType> opt1 = l1.step(a);
            if (!opt1.isPresent()) {
                throw new RuntimeException("TODO Either Exception");
            }
            l1 = opt1.get();
            dmap.put(k1, l1);
            Delta delta1 = new Delta(dmap);*/

            EAVType A = cast.val.infer();
            Either<Exception, Pair<AsyncDelta, Tree<String>>> astep =
                    this.adelta.step(s, active.role, a, A);
            if (astep.isLeft()) {
                return Either.left(astep.getLeft());
            }
            Pair<AsyncDelta, Tree<String>> astep1 = astep.getRight();

            return Either.right(Triple.of(
                    RF.asyncSystem(this.lf, actors1, queues1, EAUtil.copyOf(this.access), astep1.left),
                    get.right,
                    astep1.right
            ));
        } else {
            return Either.left(newStuck("Unsupported expr kind " + e + "in: " + this));
        }
    }

    // [E-React]
    public Either<Exception, Triple<EAAsyncSystem, Tree<String>, Tree<String>>> react(
            EAPid p, EASid s, EAMsg m) {

        EACActor c = this.actors.get(p); // p.equals(c.pid)

        EAGlobalQueue queue = this.queues.get(s);
        Either<Exception, Triple<EACActor, EAGlobalQueue, Tree<String>>> react =
                c.react(m, queue);
        if (react.isLeft()) {
            return Either.left(react.getLeft());
        }
        Triple<EACActor, EAGlobalQueue, Tree<String>> get = react.getRight();
        LinkedHashMap<EAPid, EACActor> actors1 = EAUtil.copyOf(this.actors);
        actors1.put(p, get.left);
        LinkedHashMap<EASid, EAGlobalQueue> queues1 = EAUtil.copyOf(this.queues);
        queues1.put(s, get.mid);

        LRecv a = this.lf.LRecv(null, m.snd, new SigLit(m.op, Payload.EMPTY_PAYLOAD));  // from foo  // FIXME EMPTY_PAY

        /*LinkedHashMap<Pair<EASid, Role>, EALType> dmap = EAUtil.copyOf(this.annots.map);
        Pair<EASid, Role> k2 = new Pair<>(s, m.rcv);
        EALType l2 = this.annots.map.get(k2);
        Optional<EALType> opt2 = l2.step(a);
        if (!opt2.isPresent()) {
            throw new RuntimeException("TODO Either Exception");
        }
        l2 = opt2.get();
        dmap.put(k2, l2);
        Delta delta1 = new Delta(dmap);*/

        EAVType A = m.data.infer();
        Either<Exception, Pair<AsyncDelta, Tree<String>>> astep =
                this.adelta.step(s, m.rcv, a, A);
        if (astep.isLeft()) {
            return Either.left(astep.getLeft());
        }
        Pair<AsyncDelta, Tree<String>> astep1 = astep.getRight();

        return Either.right(Triple.of(
                new EAAsyncSystem(this.lf, actors1, queues1, EAUtil.copyOf(this.access),
                        astep1.left),
                get.right,
                astep1.right
        ));
    }

    // [E-Init]
    public Either<Exception, Triple<EAAsyncSystem, Tree<String>, Tree<String>>> init(
            Pair<EAEAPName, Map<EAPid, Pair<Role, EAIota>>> inits) {

        if (!this.access.containsKey(inits.left)) {
            return Either.left(newStuck("Unknown access point " + inits.left + ": " + this.access));
        }

        // Remove iotas rom access and iotas -- n.b. iotas static mutable
        LinkedHashMap<EAEAPName, Map<Role, Pair<EALType, List<EAIota>>>> access1 =
                EAUtil.copyOf(this.access);
        Map<Role, Pair<EALType, List<EAIota>>> rs = EAUtil.copyOf(access1.get(inits.left));
        for (Pair<Role, EAIota> v : inits.right.values()) {
            if (!rs.containsKey(v.left)) {
                return Either.left(newStuck("Unknown role " + v.left
                        + " for ap " + inits.left + ": " + rs));
            }
            Pair<EALType, List<EAIota>> foo = rs.get(v.left);
            List<EAIota> tmp = EAUtil.copyOf(foo.right);
            if (!tmp.contains(v.right)) {
                return Either.left(newStuck("Iota not present " + v.right
                        + " for role " + v.left + ": " + tmp));
            }
            tmp.remove(v.right);
            rs.put(v.left, Pair.of(foo.left, tmp));

            if (!EAAsyncSystem.iotas.containsKey(v.right)) {
                throw new RuntimeException("Shouldn't get here: "
                        + EAAsyncSystem.iotas + " ,, " + v.right);
            }
            EAAsyncSystem.iotas.remove(v.left);
        }
        access1.put(inits.left, rs);

        if (inits.right.keySet().stream()
                .anyMatch(x -> this.actors.get(x).T.getMode() != EAThreadMode.IDLE)) {
            return Either.left(newStuck("Actors not all idle: " + this.actors));
        }

        EASid sid = newSid();

        // Replace actors with active session threads
        LinkedHashMap<EAPid, EACActor> actors1 = EAUtil.copyOf(this.actors);
        for (Map.Entry<EAPid, Pair<Role, EAIota>> x : inits.right.entrySet()) {
            EAPid pid = x.getKey();
            if (!this.actors.containsKey(pid)) {
                return Either.left(newStuck("Unknown pid " + pid + ": " + this.actors));
            }
            Pair<Role, EAIota> iota = x.getValue();
            EACActor idle = actors1.get(pid);
            if (!idle.rho.containsKey(iota.right)) {
                return Either.left(newStuck("Unknown iota " + iota.right + ": " + idle.rho));
            }
            EAComp M = idle.rho.get(iota.right);

            EATSession T1 = RF.sessionThread(M, sid, iota.left);
            LinkedHashMap<EAIota, EAComp> rho1 = EAUtil.copyOf(idle.rho);
            rho1.remove(iota.right);
            EACActor active = RF.actor(pid, T1, EAUtil.copyOf(idle.sigma), rho1, idle.state);
            actors1.put(pid, active);
        }

        // Add session queue
        if (this.queues.containsKey(sid)) {
            return Either.left(newStuck("Duplicate session id " + sid + ": " + this.queues));
        }
        LinkedHashMap<EASid, EAGlobalQueue> queues1 = EAUtil.copyOf(this.queues);
        queues1.put(sid, new EAGlobalQueue(sid));

        // Add to delta annots
        LinkedHashMap<Pair<EASid, Role>, EALType> types1 = EAUtil.copyOf(this.adelta.types);
        LinkedHashMap<EASid, List<EAMsgType>> dqueues1 = EAUtil.copyOf(this.adelta.queues);
        for (Pair<Role, EAIota> x : inits.right.values()) {
            types1.put(Pair.of(sid, x.left), this.access.get(inits.left).get(x.left).left);
            dqueues1.put(sid, EAUtil.listOf());
        }
        AsyncDelta delta1 = new AsyncDelta(types1, dqueues1);

        String pre = inits.left + "(" + this.access.get(inits.left) + ") | " +
                inits.right.keySet().stream().map(x -> {
                    EACActor p = this.actors.get(x);
                    return "<" + p.T + ", " + p.sigma + ", " + p.rho + ">";
                }).collect(Collectors.joining(" | "));
        String post = inits.left + "(" + access1.get(inits.left) + ") | " +
                inits.right.keySet().stream().map(x -> {
                    EACActor p = actors1.get(x);
                    return "<" + p.T + ", " + p.sigma + ", " + p.rho + ">";
                }).collect(Collectors.joining(" | ")) +
                " | " + queues1.get(sid);
        return Either.right(Triple.of(
                new EAAsyncSystem(this.lf, actors1, queues1, access1, delta1),
                Tree.of("[E-Init] " + pre + " " + ConsoleColors.RIGHTARROW + " " + post),
                null  // XXX TODO -- or fine when no delta deriv?
        ));
    }

    /* ... */

    public static Exception newStuck(String txt) {
        return new Exception(txt);
    }

    /*public Map<EAPid, EAPConfig> getConfigs() {
        return Collections.unmodifiableMap(this.configs);
    }*/

    @Override
    public String toString() {
        return toString("");
    }

    public String toString(String indent) {
        return indent + "[" //+ annots=\n" + this.annots.map
                + this.actors.entrySet().stream().map(Object::toString).collect(Collectors.joining("\n" + indent + " "))
                + "\n" + indent + " " + (this.queues.isEmpty() ? "{}" : this.queues.entrySet().stream().map(Object::toString).collect(Collectors.joining("\n" + indent + " ")))
                + "\n" + indent + " " + (this.access.isEmpty() ? "{}" : this.access)
                + "\n" + indent + " " + this.adelta
                + "\n" + indent + "]";
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) { return true; }
        if (o == null || getClass() != o.getClass()) { return false; }
        EAAsyncSystem them = (EAAsyncSystem) o;
        return this.adelta.equals(them.adelta)
                && this.actors.equals(them.actors)
                && this.queues.equals(them.queues)
                && this.access.equals(them.access);
    }

    @Override
    public int hashCode() {
        int hash = 71809;
        hash = 31 * hash + this.adelta.hashCode();
        hash = 31 * hash + this.actors.hashCode();
        hash = 31 * hash + this.queues.hashCode();
        hash = 31 * hash + this.access.hashCode();
        return hash;

    }
}
