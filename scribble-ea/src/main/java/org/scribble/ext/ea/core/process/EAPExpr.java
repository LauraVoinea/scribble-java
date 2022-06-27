package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;

import java.util.Map;
import java.util.Set;

// "Computation"
public interface EAPExpr extends EAPTerm {

    boolean canBeta();
    EAPExpr beta();  // !!! CHECKME deterministic

    EAPExpr subs(@NotNull Map<EAPVar, EAPVal> m);
    EAPExpr recon(@NotNull EAPExpr old, @NotNull EAPExpr neww);  // A subs for Expr (cf. Val)

    Set<EAPVar> getFreeVars();
    boolean isGround();

    EAPExpr getFoo();  // deterministic
}

