package org.scribble.ext.ea.core.term.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.EAFuncName;
import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.term.expr.EAERec;
import org.scribble.ext.ea.core.term.expr.EAExpr;
import org.scribble.ext.ea.core.term.expr.EAEVar;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.EAPPair;

import java.util.Map;
import java.util.Set;

// "Computation"
public interface EAComp extends EATerm {

    // CHECKME still needed? or deprecate
    EALType infer(Gamma gamma);

    EAPPair<EAVType, EALType> type(Gamma gamma, EALType pre);

    // ->_M -- config independent M eval
    boolean canBeta();

    EAComp beta();  // !!! CHECKME deterministic

    EAComp subs(@NotNull Map<EAEVar, EAExpr> m);

    EAComp fsubs(@NotNull Map<EAFuncName, EAERec> m);

    // CHECKME needed?
    EAComp recon(@NotNull EAComp old, @NotNull EAComp neww);  // A subs for Expr (cf. Val)

    Set<EAEVar> getFreeVars();

    // FIXME separate "is ground" (cf. EAPSystem.reduce) from "is finished value" (cf. EAPLet.canBeta)
    boolean isGround();

    default boolean isGroundValueReturn() {
        return false;
    }

    // Extract the (nested) "statically reducible part" CANDIDATE for config reduction -- e.g., send can only be a candidate (so app/let/etc don't check canBeta for foo -- EAPActiveThread.canStep checks canBeta on relevant foo, but could refactor some canBeta into getFoo)
    //boolean canFoo();
    EAComp getConfigRedexCandidate();  // deterministic(?)  // doesn't check canBeta, EAPActiveThread.canStep checks it as necessary

    EAComp configStep();
}

