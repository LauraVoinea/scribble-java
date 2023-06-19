package org.scribble.ext.ea.core.type.session.local;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.local.LRecv;
import org.scribble.core.type.session.local.LSend;
import org.scribble.core.type.session.local.LType;
import org.scribble.ext.ea.core.runtime.EASid;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.ConsoleColors;
import org.scribble.ext.ea.util.EAUtil;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

public class AsyncDelta {

    // unmod LinkedHashMaps
    @NotNull public final Map<Pair<EASid, Role>, EALType> types;
    @NotNull public final Map<EASid, List<EAMsgType>> queues;  // cf. EAGlobalQueue... factor out?

    public AsyncDelta() {
        this(EAUtil.mapOf(), EAUtil.mapOf());
    }

    public AsyncDelta(LinkedHashMap<Pair<EASid, Role>, EALType> types,
                      LinkedHashMap<EASid, List<EAMsgType>> queues) {
        if (types.keySet().stream().anyMatch(x -> !queues.keySet().contains(x.left))) {  // TODO also check other direction
            throw new RuntimeException("Session in types but not in queues: "
                    + "\n\t" + types + "\n\t" + queues);
        }
        this.types = EAUtil.umodCopyOf(types);
        this.queues = EAUtil.umodCopyOfList(queues);
    }

    /* ... */

    // FIXME deprecate base LSend etc (incompatible with EAVType) -- !!! LSend.src is "self" (not actual role)
    public Either<Exception, Pair<AsyncDelta, Tree<String>>> step(
            EASid s, Role snd, LSend a, EAVType A) {

        Pair<EASid, Role> ep = Pair.of(s, snd);
        if (!this.types.containsKey(ep) || !queues.containsKey(s)) {
            return Either.left(newStuck("Unknown session/role " + s
                    + "[" + snd + "]: " + this));
        }
        EALType type = this.types.get(ep);
        /*if (!(type instanceof EALOutType)) {  // redundant with step below -- XXX rec
            return Either.left(newStuck(a + " but not output type: " + type));
        }*/
        Optional<EALType> step = type.step(a);
        if (!step.isPresent()) {
            return Either.left(newStuck("Could not step " + a + ": " + type));
        }
        EALType get = step.get();
        LinkedHashMap<Pair<EASid, Role>, EALType> types1 = EAUtil.copyOf(this.types);
        types1.put(ep, get);
        LinkedHashMap<EASid, List<EAMsgType>> queues1 = append(queues, s, snd, a, A);
        AsyncDelta succ = new AsyncDelta(types1, queues1);
        return Either.right(Pair.of(succ, Tree.of(
                toLTSJudgeString("[Lbl-Sync-Send]", this, snd, a, A, succ))));
    }

    public static String toLTSJudgeString(
            String tag, AsyncDelta d1, Role self, LType a, EAVType A, AsyncDelta d2) {
        return tag + "  " + d1 + " -" + self + ":" + a + "(" + A + ")"
                + ConsoleColors.RIGHTARROW + " " + d2;
    }

    public static Exception newStuck(String txt) {
        return new Exception("Stuck: " + (txt.isEmpty() ? "" : txt + ": "));
    }

    public Either<Exception, Pair<AsyncDelta, Tree<String>>> step(
            EASid s, Role dst, LRecv a, EAVType A) {

        Pair<EASid, Role> ep = Pair.of(s, dst);
        if (!this.types.containsKey(ep) || !queues.containsKey(s)) {
            return Either.left(newStuck("Unknown session/role " + s
                    + "[" + dst + "]: " + this));
        }
        EALType type = this.types.get(ep);
        /*if (!(type instanceof EALInType)) {  // redundant with step below -- XXX rec
            return Either.left(newStuck(a + " but not input type: " + type));
        }*/
        List<EAMsgType> queue = this.queues.get(s);
        Optional<EAMsgType> fst = queue.stream().filter(x -> x.snd.equals(a.src)
                && x.rcv.equals(dst) && x.op.equals(a.msg.getId())).findFirst();
        if (!fst.isPresent()) {
            return Either.left(newStuck("No such message for " + dst + ": " + a + "\nin " + queue));
        }
        Optional<EALType> step = type.step(a);
        if (!step.isPresent()) {
            return Either.left(newStuck("Could not step " + a + ": " + type));
        }
        EALType get = step.get();
        LinkedHashMap<Pair<EASid, Role>, EALType> types1 = EAUtil.copyOf(this.types);
        types1.put(ep, get);
        LinkedHashMap<EASid, List<EAMsgType>> queues1 = removeFirst(queues, s, dst, a, A);
        AsyncDelta succ = new AsyncDelta(types1, queues1);
        return Either.right(Pair.of(succ, Tree.of(
                toLTSJudgeString("[Lbl-Sync-Recv]", this, dst, a, A, succ))));
    }

    // Cf. EAGlobalQueue
    // Return not immut
    protected static LinkedHashMap<EASid, List<EAMsgType>> append(
            Map<EASid, List<EAMsgType>> queues, EASid s, Role snd, LSend a, EAVType A) {
        LinkedHashMap<EASid, List<EAMsgType>> copy = EAUtil.copyOfList(queues);
        List<EAMsgType> tmp = copy.get(s);
        tmp.add(new EAMsgType(snd, a.dst, (Op) a.msg.getId(), A));
        return copy;
    }

    // Return not immut -- return copy if not present
    protected static LinkedHashMap<EASid, List<EAMsgType>> removeFirst(
            Map<EASid, List<EAMsgType>> queues, EASid s, Role rcv, LRecv a, EAVType A) {
        LinkedHashMap<EASid, List<EAMsgType>> copy = EAUtil.copyOfList(queues);
        List<EAMsgType> tmp = copy.get(s);
        tmp.remove(new EAMsgType(a.src, rcv, (Op) a.msg.getId(), A));  // Removes first
        return copy;
    }

    /* ... */

    @Override
    public String toString() {
        return "[" +
                this.types.entrySet().stream().map(x -> x.getKey() + "=" + x.getValue())
                        .collect(Collectors.joining(", ")) +
                (!this.types.isEmpty() ? ", " : "") +
                this.queues.entrySet().stream().map(x -> x.getKey() + "=" + x.getValue())
                        .collect(Collectors.joining(", ")) +
                "]";
    }

    /* equals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        AsyncDelta them = (AsyncDelta) o;
        return this.types.equals(them.types) && this.queues.equals(them.queues);
    }

    @Override
    public int hashCode() {
        int hash = 3121;
        hash = 31 * hash + this.types.hashCode();
        hash = 31 * hash + this.queues.hashCode();
        return hash;
    }
}
