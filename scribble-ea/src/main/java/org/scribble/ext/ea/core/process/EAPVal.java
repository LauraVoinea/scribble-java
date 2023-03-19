package org.scribble.ext.ea.core.process;

import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EAValType;

import java.util.Map;
import java.util.Set;

// !!! "values" vs. ground vals  -- XXX FIXME rename like "pure exprs"
public interface EAPVal extends EAPTerm {
    //EAPExpr {  !!! values should be exprs?

    boolean canBeta();

    EAPVal beta();

    EAValType type(Gamma gamma);

    EAPVal subs(Map<EAPVar, EAPVal> m);

    EAPVal fsubs(Map<EAPFuncName, EAPRec> m);

    Set<EAPVar> getFreeVars();

    // !!! cf. "value"
    default boolean isGround() {  // FIXME override hack for var when it's actually an fname
        return getFreeVars().isEmpty();
    }
}
