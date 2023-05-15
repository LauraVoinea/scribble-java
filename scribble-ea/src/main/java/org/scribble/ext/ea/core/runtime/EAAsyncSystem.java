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
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.Delta;
import org.scribble.ext.ea.core.type.session.local.EALEndType;
import org.scribble.ext.ea.core.type.session.local.EALType;
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

    @NotNull public final Delta annots;
    //@NotNull public final ...Global...  // project, chec against original annots (implies safety), and type check actors

    // Unmod LinkedHashMaps
    @NotNull public final Map<EAPid, EACActor> actors;  // keyset is all pids in system  // pids no longer in formal defs but useful in implementation
    @NotNull public final Map<EASid, EAGlobalQueue> queues;  // keyset is all sids in system

    public EAAsyncSystem(
            LTypeFactory lf,
            Delta annots,
            LinkedHashMap<EAPid, EACActor> actors,
            LinkedHashMap<EASid, EAGlobalQueue> queues) {

        if (actors.entrySet().stream().anyMatch(x -> !x.getKey().equals(x.getValue().pid))) {
            throw new RuntimeException("Invalid pid/config mapping: " + actors);
        }
        if (queues.entrySet().stream().anyMatch(x -> !x.getKey().equals(x.getValue().sid))) {
            throw new RuntimeException("Invalid sid/queue mapping: " + queues);
        }
        this.lf = lf;
        this.annots = annots;
        this.actors = EAUtil.umodCopyOf(actors);
        this.queues = EAUtil.umodCopyOf(queues);
    }

    /* ... */

    // !!! TODO "safety property" -- global+projection
    public Either<Exception, List<Tree<String>>> type() {
        Gamma empty = new Gamma();
        List<Tree<String>> res = new LinkedList<>();
        Set<Pair<EASid, Role>> todo = new HashSet<>(this.annots.map.keySet());
        for (EACActor c : this.actors.values()) {
            LinkedHashSet<Pair<EASid, Role>> eps = c.getEndpoints();
            LinkedHashMap<Pair<EASid, Role>, EALType> tmp = new LinkedHashMap<>();
            for (Pair<EASid, Role> p : eps) {
                if (!todo.contains(p)) {
                    return Either.left(new Exception("Unknown endpoint or already used: " + p));
                }
                todo.remove(p);
                // !!! TODO: Delta disjoint union op
                tmp.put(p, this.annots.map.get(p));  // !!! splits outer Delta (not an actual name restriction) -- cf. cf. T-Session introduce Delta', T-Par split Delta
            }
            Either<Exception, Tree<String>> t = c.type(empty, new Delta(tmp));
            if (t.isLeft()) {
                return Either.left(t.getLeft());
            }
            res.add(t.getRight());
        }
        if (todo.stream().anyMatch(x -> !this.annots.map.get(x).equals(EALEndType.END))) {
            return Either.left(new Exception("Endpoints not implemented: "
                    + todo + "\n\tannots = " + this.annots));
        }
        return Either.right(res);
    }

    /* ... */

    public Map<EAPid, Map<EASid, Either<EAComp, EAMsg>>> getSteppable() {
        Map<EAPid, Map<EASid, Either<EAComp, EAMsg>>> res = EAUtil.mapOf();
        for (Map.Entry<EAPid, EACActor> ep : this.actors.entrySet()) {
            EAPid pid = ep.getKey();
            EACActor C = ep.getValue();
            for (Map.Entry<EASid, EAGlobalQueue> es : this.queues.entrySet()) {
                EASid sid = es.getKey();
                EAGlobalQueue queue = es.getValue();
                Optional<Either<EAComp, EAMsg>> opt = C.getStepSubexprsE(queue);
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
        }
        return res;
    }

    // [E-Send], [E-Suspend], [E-Reset], [E-Lift]
    public Either<Exception, Pair<EAAsyncSystem, Tree<String>>> step(
            EAPid p, EASid s, EAComp e) {  // n.b. beta is deterministic

        EACActor c = this.actors.get(p); // p.equals(c.pid)

        // Session typing (annots) unchanged -- s not used
        if (e instanceof EAMSuspend  // [E-Suspend]
                || e instanceof EAMReturn  // [E-Reset]
                || e instanceof EAMApp || e instanceof EAMLet || e instanceof EAMIf)  // [E-Lift]
        {
            Either<Exception, Pair<EACActor, Tree<String>>> step = c.stepAsync0(e);
            return step.mapRight(x -> {
                LinkedHashMap<EAPid, EACActor> actors1 = EAUtil.copyOf(this.actors);
                actors1.put(p, x.left);
                LinkedHashMap<EASid, EAGlobalQueue> queues = EAUtil.copyOf(this.queues);
                return Pair.of(
                        new EAAsyncSystem(this.lf, this.annots, actors1, queues),
                        x.right
                );
            });

        } else if (e instanceof EAMSend) {  // [E-Send]
            EATActive active = (EATActive) c.T;
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

            LinkedHashMap<Pair<EASid, Role>, EALType> dmap = EAUtil.copyOf(this.annots.map);
            Pair<EASid, Role> k1 = new Pair<>(s, active.role);
            EALType l1 = this.annots.map.get(k1);
            LSend ls = this.lf.LSend(null, new SigLit(cast.op, Payload.EMPTY_PAYLOAD), cast.dst);  // FIXME EMPTY_PAY
            Optional<EALType> opt1 = l1.step(ls);
            if (!opt1.isPresent()) {
                throw new RuntimeException("TODO Either Exception");
            }
            l1 = opt1.get();
            dmap.put(k1, l1);
            Delta delta1 = new Delta(dmap);

            return Either.right(Pair.of(
                    new EAAsyncSystem(this.lf, delta1, actors1, queues1),
                    get.right
            ));

        } else {
            return Either.left(newStuck("Unsupported expr kind " + e + "in: " + this));
        }
    }

    public static Exception newStuck(String txt) {
        return new Exception(txt);
    }

    // [E-React]
    public Either<Exception, Pair<EAAsyncSystem, Tree<String>>> react(
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

        LinkedHashMap<Pair<EASid, Role>, EALType> dmap = EAUtil.copyOf(this.annots.map);
        Pair<EASid, Role> k2 = new Pair<>(s, m.rcv);
        EALType l2 = this.annots.map.get(k2);
        LRecv lr = this.lf.LRecv(null, m.snd, new SigLit(m.op, Payload.EMPTY_PAYLOAD));  // from foo  // FIXME EMPTY_PAY
        Optional<EALType> opt2 = l2.step(lr);
        if (!opt2.isPresent()) {
            throw new RuntimeException("TODO Either Exception");
        }
        l2 = opt2.get();
        dmap.put(k2, l2);
        Delta delta1 = new Delta(dmap);

        return Either.right(Pair.of(
                new EAAsyncSystem(this.lf, delta1, actors1, queues1),
                get.right
        ));
    }

    /*public Map<EAPid, EAPConfig> getConfigs() {
        return Collections.unmodifiableMap(this.configs);
    }*/

    @Override
    public String toString() {
        return "[annots=\n" + this.annots.map + "\nconfigs=\n"
                + this.actors.entrySet().stream().map(Object::toString).collect(Collectors.joining("\n"))
                + "\n" + this.queues.entrySet().stream().map(Object::toString).collect(Collectors.joining("\n"))
                + "]";
    }
}
