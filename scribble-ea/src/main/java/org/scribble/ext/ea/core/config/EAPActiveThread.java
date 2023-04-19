package org.scribble.ext.ea.core.config;

import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.term.*;
import org.scribble.ext.ea.core.term.expr.EAEUnit;
import org.scribble.ext.ea.core.term.process.*;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.Delta;
import org.scribble.ext.ea.core.type.session.local.EALEndType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.core.type.value.EAVUnitType;
import org.scribble.ext.ea.util.EAPPair;
import org.scribble.util.Pair;

import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

public class EAPActiveThread implements EAPThreadState {

    public final EAComp expr;
    public final EAPSid sid;
    public final Role role;

    protected EAPActiveThread(EAComp expr, EAPSid sid, Role role) {
        this.expr = expr;
        this.sid = sid;
        this.role = role;
    }

    // return Pid set is "partners" (sync actions) -- TODO for (optimising?) init
    // gets static foo CANDIDATE redexes and checks canBeta on them as needed
    //
    // ...No step in EAPActiveThread -- most cases don't reduce (just) the expr/thread, but rather change whole config(s), so leave to EAPConfig
    // Maybe refactor canStep to EAPConfig
    public Pair<Boolean, Set<EAPPid>> canStep(EAPSystem sys) {
        EAComp foo = this.expr.getConfigRedexCandidate();
        // top-level return ()
        if (foo instanceof EAPReturn) {
            if (this.expr instanceof EAPReturn && ((EAPReturn) this.expr).val.equals(EAEUnit.UNIT)) {
                return new Pair<>(true, Collections.emptySet());
            }
            // let x <= return V in ... handled by let as foo (LiftM beta) -- !!! HERE now additional beta case: let x <= return V where V.canBeta (return V is the foo)
            return new Pair<>(foo.canBeta(), Collections.emptySet());
        } else if (foo instanceof EAPSend) {
            EAPSend cast = (EAPSend) foo;
            Optional<Map.Entry<EAPPid, EAPConfig>> fst =
                    sys.configs.entrySet().stream().filter(x -> {
                                EAPConfig v = x.getValue();
                                return v.T.isIdle() && v.sigma.keySet().stream().anyMatch(y ->
                                        y.left.equals(this.sid) && y.right.equals(cast.dst)
                                                && x.getValue().sigma.get(y).role.equals(this.role));
                            }
                    ).findFirst();
            if (fst.isPresent()) {
                EAPPid key = fst.get().getKey();
                return new Pair<>(true, Set.of(key));
            } else {
                return new Pair<>(false, Collections.emptySet());
            }
        }
        // Other non-beta cases
        else if (foo instanceof EAPSuspend) {  // Other non-beta cases
            return new Pair<>(true, Collections.emptySet());
        }
        // LiftM beta cases
        else if (foo instanceof EAPApp || foo instanceof EAPLet || foo instanceof EAPIf || foo instanceof EAPIf) {
            return new Pair<>(foo.canBeta(), Collections.emptySet());
        }
        throw new RuntimeException("Unknown foo: " + foo);
    }

    // [TT-Sess]
    @Override
    public void type(Gamma gamma, Delta delta) {
        if (delta.map.size() != 1) {
            throw new RuntimeException("Invalid Delta: " + delta);
        }
        EALType pre = delta.map.get(new Pair<>(this.sid, this.role));
        if (pre == null) {
            throw new RuntimeException("Unknown endpoint: "
                    + endpointToString(this.sid, this.role));
        }
        Pair<EAVType, EALType> res = this.expr.type(gamma, pre);
        if (!res.equals(new EAPPair<>(EAVUnitType.UNIT, EALEndType.END))) {
            throw new RuntimeException("Badly typed: " + this + " : "
                    + res.left + " <| " + res.right);
        }
    }

    /* aux */

    public static String endpointToString(EAPSid sid, Role role) {
        return sid + "[" + role + "]";
    }

    @Override
    public String toString() {
        return "(" + this.expr + ")@" + endpointToString(this.sid, this.role);
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPActiveThread eaVar = (EAPActiveThread) o;
        return eaVar.canEquals(this);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPActiveThread;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.ACTIVE_THREAD;
        hash = 31 * hash;
        hash = 31 * this.expr.hashCode();
        hash = 31 * this.sid.hashCode();
        hash = 31 * this.role.hashCode();
        return hash;
    }
}
