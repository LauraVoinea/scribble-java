package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.util.EAPPair;

import java.util.Map;
import java.util.Set;

// "Computation"
public interface EAPExpr extends EAPTerm {

    // CHECKME still needed? or deprecate
    EALType infer(Gamma gamma);

    EAPPair<EAValType, EALType> type(Gamma gamma, EALType pre);

    // ->_M -- config independent M eval
    boolean canBeta();

    EAPExpr beta();  // !!! CHECKME deterministic

    EAPExpr subs(@NotNull Map<EAPVar, EAPVal> m);

    EAPExpr fsubs(@NotNull Map<EAPFuncName, EAPRec> m);

    // CHECKME needed?
    EAPExpr recon(@NotNull EAPExpr old, @NotNull EAPExpr neww);  // A subs for Expr (cf. Val)

    Set<EAPVar> getFreeVars();

    // FIXME separate "is ground" (cf. EAPSystem.reduce) from "is finished value" (cf. EAPLet.canBeta)
    boolean isGround();

    default boolean isGroundValueReturn() {
        return false;
    }

    // Extract the (nested) "statically reducible part" CANDIDATE for config reduction -- e.g., send can only be a candidate (so app/let/etc don't check canBeta for foo -- EAPActiveThread.canStep checks canBeta on relevant foo, but could refactor some canBeta into getFoo)
    //boolean canFoo();
    EAPExpr getFoo();  // deterministic(?)  // doesn't check canBeta, EAPActiveThread.canStep checks it as necessary

    EAPExpr foo();
}

