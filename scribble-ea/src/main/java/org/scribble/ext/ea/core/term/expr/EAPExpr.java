package org.scribble.ext.ea.core.term.expr;

import org.scribble.ext.ea.core.term.EAPFuncName;
import org.scribble.ext.ea.core.term.EAPTerm;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EAValType;

import java.util.Map;
import java.util.Set;

// "pure" exprs (no I/O)
public interface EAPExpr extends EAPTerm {
    //EAPExpr {  !!! values should be exprs?

    EAValType infer();  // not intended to work for single var -- for use on ground vals at config level

    boolean canBeta();

    EAPExpr beta();

    EAValType type(Gamma gamma);

    EAPExpr subs(Map<EAPVar, EAPExpr> m);

    EAPExpr fsubs(Map<EAPFuncName, EAPRec> m);

    Set<EAPVar> getFreeVars();

    // !!! cf. "value"
    default boolean isGround() {  // FIXME override hack for var when it's actually an fname
        return getFreeVars().isEmpty();
    }
}
