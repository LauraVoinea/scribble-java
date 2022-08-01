package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.EALOutType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.util.Pair;

import java.util.Map;
import java.util.Set;

// "Computation"
public interface EAPExpr extends EAPTerm {

    EALType infer(Gamma gamma);
    Pair<EAValType, EALType> type(Gamma gamma, EALType pre);

    boolean canBeta();
    EAPExpr beta();  // !!! CHECKME deterministic

    EAPExpr subs(@NotNull Map<EAPVar, EAPVal> m);
    EAPExpr recon(@NotNull EAPExpr old, @NotNull EAPExpr neww);  // A subs for Expr (cf. Val)

    Set<EAPVar> getFreeVars();
    boolean isGround();

    // Extract the (nested) "reducible part"
    EAPExpr getFoo();  // deterministic
}

