package org.scribble.ext.ea.core.process;

import java.util.Map;
import java.util.Set;

// !!! "values" vs. ground vals
public interface EAPVal extends EAPTerm {
    //EAPExpr {  !!! values should be exprs?

    EAPVal subs(Map<EAPVar, EAPVal> m);

    Set<EAPVar> getFreeVars();

    // !!! cf. "value"
    default boolean isGround() {
        return getFreeVars().isEmpty();
    }
}
