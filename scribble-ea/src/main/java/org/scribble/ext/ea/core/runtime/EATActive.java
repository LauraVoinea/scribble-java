package org.scribble.ext.ea.core.runtime;

import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.runtime.config.EACActor;
import org.scribble.ext.ea.core.term.*;
import org.scribble.ext.ea.core.term.comp.*;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.Delta;
import org.scribble.ext.ea.core.type.session.local.EALEndType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

import java.util.*;

public class EATActive implements EAThread {

    public final EAComp comp;
    public final EASid sid;
    public final Role role;

    protected EATActive(EAComp comp, EASid sid, Role role) {
        this.comp = comp;
        this.sid = sid;
        this.role = role;
    }

    // return Pid set is "partners" (sync actions) -- TODO for (optimising?) init
    // gets static foo CANDIDATE redexes and checks canBeta on them as needed
    //
    // ...No step in EAPActiveThread -- most cases don't reduce (just) the expr/thread, but rather change whole config(s), so leave to EAPConfig
    // Maybe refactor canStep to EAPConfig
    public Pair<Boolean, Set<EAPid>> canActorReduce(EASystem sys) {
        EAComp foo = this.comp.getStepSubexprE();
        // top-level return ()
        if (foo instanceof EAMReturn) {
            //if (this.expr instanceof EAMReturn && ((EAMReturn) this.expr).val.equals(EAEUnit.UNIT)) {
            if (this.comp instanceof EAMReturn && this.comp.isGroundValueReturn()) {
                return new Pair<>(true, Collections.emptySet());
            }
            // let x <= return V in ... handled by let as foo (LiftM beta) -- !!! HERE now additional beta case: let x <= return V where V.canBeta (return V is the foo)
            return new Pair<>(foo.canBeta(), Collections.emptySet());
        } else if (foo instanceof EAMSend) {
            EAMSend cast = (EAMSend) foo;
            Optional<Map.Entry<EAPid, EACActor>> fst =
                    sys.actors.entrySet().stream().filter(x -> {
                                EACActor v = x.getValue();
                                return v.T.isIdle() && v.sigma.keySet().stream().anyMatch(y ->
                                        y.left.equals(this.sid) && y.right.equals(cast.dst)
                                                && x.getValue().sigma.get(y).role.equals(this.role));
                            }
                    ).findFirst();
            if (fst.isPresent()) {
                EAPid key = fst.get().getKey();
                return new Pair<>(true, Set.of(key));
            } else {
                return new Pair<>(false, Collections.emptySet());
            }
        }
        // Other non-beta cases
        else if (foo instanceof EAMSuspend) {  // Other non-beta cases
            return new Pair<>(true, Collections.emptySet());
        }
        // LiftM beta cases
        else if (foo instanceof EAMApp || foo instanceof EAMLet || foo instanceof EAMIf || foo instanceof EAMIf) {
            return new Pair<>(foo.canBeta(), Collections.emptySet());
        }
        throw new RuntimeException("Unknown foo: " + foo);
    }

    // [TT-Sess]
    @Override
    public Either<Exception, Tree<String>> type(GammaState gamma, Delta delta) {
        if (delta.map.size() != 1) {
            //throw new RuntimeException("Invalid Delta: " + delta);
            return Either.left(new Exception("Invalid Delta: " + delta));
        }
        Pair<EASid, Role> c = Pair.of(this.sid, this.role);
        EALType pre = delta.map.get(c);
        if (pre == null) {
            //throw new RuntimeException("Unknown endpoint: " + endpointToString(this.sid, this.role));
            return Either.left(new Exception("Unknown endpoint: " + endpointToString(this.sid, this.role)));
        }
        //Pair<EAVType, EALType> res = this.expr.type(gamma, pre);
        Either<Exception, Pair<Pair<EAVType, EALType>, Tree<String>>> t = this.comp.type(gamma, pre);
        if (t.isLeft()) {
            //throw new RuntimeException(t.getLeft().get());
            return Either.left(t.getLeft());
        }
        Pair<Pair<EAVType, EALType>, Tree<String>> pp = t.getRight();
        Pair<EAVType, EALType> res = pp.left;
        ////if (!res.equals(new Pair<>(EAVUnitType.UNIT, EALEndType.END))) {
        ////if (!res.equals(new Pair<>(gamma.svarType, EALEndType.END))) {
        Optional<EAVType> u = EAVType.unify(res.left, gamma.svarType);
        if (!u.isPresent() || !u.get().equals(gamma.svarType) || !res.right.equals(EALEndType.END)) {
            //throw new RuntimeException("Badly typed: " + this + " : " + res.left + " <| " + res.right);
            return Either.left(new Exception("Badly typed: " + this + " : " + res.left + " <| " + res.right));
        }
        return Either.right(new Tree<>(
                "[TT-Sess] " + toJudgementString(gamma, delta), pp.right));
    }

    /* aux */

    public static String endpointToString(EASid sid, Role role) {
        return sid + "[" + role + "]";
    }

    @Override
    public String toString() {
        return "(" + this.comp + ")@" + endpointToString(this.sid, this.role);
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EATActive them = (EATActive) o;
        return this.canEquals(this) && this.comp.equals(them.comp)
                && this.sid.equals(them.sid) && this.role.equals(them.role);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EATActive;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.ACTIVE_THREAD;
        hash = 31 * hash;
        hash = 31 * this.comp.hashCode();
        hash = 31 * this.sid.hashCode();
        hash = 31 * this.role.hashCode();
        return hash;
    }
}
