package org.scribble.ext.ea.core.runtime;

import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.term.comp.*;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.Delta;
import org.scribble.ext.ea.core.type.session.local.EALEndType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

import java.util.Collections;
import java.util.Optional;
import java.util.Set;

public class EATNoSession implements EAThread {

    public final EAComp comp;

    protected EATNoSession(EAComp comp) {
        this.comp = comp;
    }

    // cf. EATSession
    public Pair<Boolean, Set<EAPid>> canActorReduce(EASystem sys) {

        EAComp foo = this.comp.getStepSubexprE();  // No-session M-context directly gets E-fragment candidate

        // top-level return () -- Q-context is both session and no-session
        if (foo instanceof EAMReturn) {
            if (this.comp instanceof EAMReturn && this.comp.isGroundValueReturn()) {
                return new Pair<>(true, Collections.emptySet());
            }

            // beta case: let x <= return V in ... handled by let as foo (LiftM beta) -- XXX cannot beta by itself
            //return new Pair<>(foo.canBeta(), Collections.emptySet());
            return new Pair<>(false, Collections.emptySet());

        }

        // Only these two (and above return) non-beta cases -- [E-Send] and [E-Suspend] hardcoded to "session" M-context
        else if (foo instanceof EAMSpawn || foo instanceof EAMRegister) {  // !!! FIXME TODO also add to EATActive (and register)
            return new Pair<>(true, Collections.emptySet());

        } else if (foo instanceof EAMSend || foo instanceof EAMSuspend) {
            return new Pair<>(false, Collections.emptySet());
        }

        // LiftM beta cases
        else if (foo instanceof EAMApp || foo instanceof EAMLet
                || foo instanceof EAMIf || foo instanceof EAMIf) {
            return new Pair<>(foo.canBeta(), Collections.emptySet());  // Same as EATActive
        }

        throw new RuntimeException("Unknown foo: " + foo);
    }

    // [TT-NoSess]
    @Override
    public Either<Exception, Tree<String>> type(GammaState gamma, Delta delta) {
        if (!delta.map.isEmpty()) {
            return Either.left(new Exception("Invalid Delta: " + delta));
        }
        Either<Exception, Pair<Pair<EAVType, EALType>, Tree<String>>> t = this.comp.type(gamma, EALEndType.END);  // Pre is end -- cf. "session" mode
        if (t.isLeft()) {
            return Either.left(t.getLeft());
        }
        Pair<Pair<EAVType, EALType>, Tree<String>> pp = t.getRight();
        Pair<EAVType, EALType> res = pp.left;
        Optional<EAVType> u = EAVType.unify(res.left, gamma.svarType);
        System.out.println(res.left + " ,, " + gamma.svarType + " ,, " + u);
        if (!u.isPresent() || !u.get().equals(gamma.svarType) || !res.right.equals(EALEndType.END)) {
            return Either.left(new Exception("Badly typed: " + this + " : " + res.left + " <| " + res.right));
        }
        return Either.right(new Tree<>(
                "[TT-NoSess] " + toJudgementString(gamma, delta), pp.right));
    }

    /* aux */

    @Override
    public EAThreadMode getMode() {
        return EAThreadMode.NO_SESSION;
    }

    @Override
    public String toString() {
        return "(" + this.comp.toString() + ")";  // !!!
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EATNoSession them = (EATNoSession) o;
        return this.canEquals(this) && this.comp.equals(them.comp);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EATNoSession;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.ACTIVE_LOCAL;
        hash = 31 * hash;
        hash = 31 * this.comp.hashCode();
        return hash;
    }
}
