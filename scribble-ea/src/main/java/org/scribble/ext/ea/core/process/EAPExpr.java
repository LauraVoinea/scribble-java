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

    EALType infer(Gamma gamma);
    EAPPair<EAValType, EALType> type(Gamma gamma, EALType pre);

    // ->_M -- config independent M eval
    boolean canBeta();
    EAPExpr beta();  // !!! CHECKME deterministic

    EAPExpr subs(@NotNull Map<EAPVar, EAPVal> m);
    EAPExpr fsubs(@NotNull Map<EAPFuncName, EAPRec> m);

    EAPExpr recon(@NotNull EAPExpr old, @NotNull EAPExpr neww);  // A subs for Expr (cf. Val)

    Set<EAPVar> getFreeVars();
    boolean isGround();

    // Extract the (nested) "reducible part" CANDIDATE for config reduction -- e.g., send can only be a candidate (so app/let/etc don't check canBeta for foo)
    //boolean canFoo();
    EAPExpr getFoo();  // deterministic
    EAPExpr foo();
}

