package org.scribble.ext.ea.core.runtime;

import org.jetbrains.annotations.NotNull;
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
import org.scribble.ext.ea.core.type.session.local.AsyncDelta;
import org.scribble.ext.ea.core.type.session.local.Delta;
import org.scribble.ext.ea.core.type.session.local.EALEndType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.EAUtil;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.ext.ea.util.Triple;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

// cf. T-Session and (nested) T-Par (missing)
// CHECKME: equiv to normal form with all \nu s at top?  sufficiently general?
public class EAAsyncSystem {

    @NotNull protected final LTypeFactory lf;
    @NotNull protected final EARuntimeFactory RF = EARuntimeFactory.factory;

    //@NotNull public final Delta annots;

    //@NotNull public final ...Global...  // project, chec against original annots (implies safety), and type check actors
    @NotNull public final AsyncDelta adelta;

    // Unmod LinkedHashMaps
    @NotNull public final Map<EAPid, EACActor> actors;  // keyset is all pids in system  // pids no longer in formal defs but useful in implementation
    @NotNull public final Map<EASid, EAGlobalQueue> queues;  // keyset is all sids in system

    @NotNull public final Map<EAEAPName, Map<Role, List<EAIota>>> access;

    public EAAsyncSystem(
            LTypeFactory lf,
            //Delta annots,
            LinkedHashMap<EAPid, EACActor> actors,
            LinkedHashMap<EASid, EAGlobalQueue> queues,
            LinkedHashMap<EAEAPName, Map<Role, List<EAIota>>> access,
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
    public Either<Exception, List<Tree<String>>> type() {
        Gamma empty = new Gamma();
        List<Tree<String>> res = new LinkedList<>();
        //Set<Pair<EASid, Role>> todo = new HashSet<>(this.annots.map.keySet());
        Set<Pair<EASid, Role>> todo = new HashSet<>(this.adelta.types.keySet());
        for (EACActor c : this.actors.values()) {
            LinkedHashSet<Pair<EASid, Role>> eps = c.getEndpoints();
            LinkedHashMap<Pair<EASid, Role>, EALType> tmp = new LinkedHashMap<>();
            for (Pair<EASid, Role> p : eps) {
                if (!todo.contains(p)) {
                    return Either.left(new Exception("Unknown endpoint or already used: " + p));
                }
                todo.remove(p);
                // !!! TODO: Delta disjoint union op
                //tmp.put(p, this.annots.map.get(p));  // !!! splits outer Delta (not an actual name restriction) -- cf. cf. T-Session introduce Delta', T-Par split Delta
                tmp.put(p, this.adelta.types.get(p));  // !!! splits outer Delta (not an actual name restriction) -- cf. cf. T-Session introduce Delta', T-Par split Delta
            }
            Either<Exception, Tree<String>> t = c.type(empty, new Delta(tmp));
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

    public Map<EAPid, Either<Map<EASid, Either<EAComp, EAMsg>>, Set<EAComp>>> getSteppable() {
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
        return tmp;
    }

    protected static int pidCounter = 1;

    // FIXME cf. exhaustive state testing
    protected int nextSpawnCounter() {
        return this.pidCounter++;
    }

    protected static int iCounter = 1;

    // FIXME cf. exhaustive state testing
    protected EAIota newIota() {
        return RF.iota("\u03b9" + iCounter++);
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
                        null  // XXX TODO
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
                    null  // XXX TODO
            ));

        } else if (e instanceof EAMRegister) {
            EAIota iota = newIota();
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
            LinkedHashMap<EAEAPName, Map<Role, List<EAIota>>> access = EAUtil.copyOf(this.access);
            if (!access.containsKey(ap)) {
                return Either.left(newStuck("Access point " + ap + " not found: " + this.access));
            }
            Map<Role, List<EAIota>> invites = EAUtil.copyOf(access.get(ap));
            List<EAIota> is = invites.containsKey(cast.role)
                    ? EAUtil.copyOf(invites.get(cast.role))
                    : EAUtil.listOf();
            is.add(iota);
            invites.put(cast.role, is);
            access.put(ap, invites);

            return Either.right(Triple.of(
                    RF.asyncSystem(this.lf, actors1, EAUtil.copyOf(this.queues), access,
                            this.adelta),
                    get.right,
                    null  // XXX TODO
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

    public static Exception newStuck(String txt) {
        return new Exception(txt);
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
        ));  // TODO astep1 deriv
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
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
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
