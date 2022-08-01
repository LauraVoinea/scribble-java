package org.scribble.ext.ea.core.process;

import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EAValType;

import java.util.Map;
import java.util.Set;

// !!! "values" vs. ground vals
public interface EAPVal extends EAPTerm {
    //EAPExpr {  !!! values should be exprs?

    EAValType type(Gamma gamma);

    EAPVal subs(Map<EAPVar, EAPVal> m);

    Set<EAPVar> getFreeVars();

    // !!! cf. "value"
    default boolean isGround() {
        return getFreeVars().isEmpty();
    }
}
