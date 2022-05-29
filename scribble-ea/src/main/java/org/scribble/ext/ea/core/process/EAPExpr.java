package org.scribble.ext.ea.core.process;

import java.util.Map;
import java.util.Set;

// "Computation"
public interface EAPExpr extends EAPTerm {

    boolean canBeta();
    EAPExpr beta();

    EAPExpr subs(Map<EAPVar, EAPVal> m);

    Set<EAPVar> getFreeVars();
}

