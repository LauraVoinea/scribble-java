package org.scribble.ext.ea.core.term.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.EAPFuncName;
import org.scribble.ext.ea.core.term.EAPTerm;
import org.scribble.ext.ea.core.term.expr.EAPRec;
import org.scribble.ext.ea.core.term.expr.EAPExpr;
import org.scribble.ext.ea.core.term.expr.EAPVar;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.util.EAPPair;

import java.util.Map;
import java.util.Set;

// "Computation"
public interface EAComp extends EAPTerm {

    // CHECKME still needed? or deprecate
    EALType infer(Gamma gamma);

    EAPPair<EAValType, EALType> type(Gamma gamma, EALType pre);

    // ->_M -- config independent M eval
    boolean canBeta();

    EAComp beta();  // !!! CHECKME deterministic

    EAComp subs(@NotNull Map<EAPVar, EAPExpr> m);

    EAComp fsubs(@NotNull Map<EAPFuncName, EAPRec> m);

    // CHECKME needed?
    EAComp recon(@NotNull EAComp old, @NotNull EAComp neww);  // A subs for Expr (cf. Val)

    Set<EAPVar> getFreeVars();

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

