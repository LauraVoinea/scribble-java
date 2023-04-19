package org.scribble.ext.ea.core.term.expr;

import org.scribble.ext.ea.core.term.EAFuncName;
import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EAVType;

import java.util.Map;
import java.util.Set;

// "pure" exprs (no I/O)
public interface EAExpr extends EATerm {
    //EAPExpr {  !!! values should be exprs?

    EAVType infer();  // not intended to work for single var -- for use on ground vals at config level

    boolean canBeta();

    EAExpr beta();

    EAVType type(Gamma gamma);

    EAExpr subs(Map<EAEVar, EAExpr> m);

    EAExpr fsubs(Map<EAFuncName, EAERec> m);

    Set<EAEVar> getFreeVars();

    // !!! cf. "value"
    default boolean isGround() {  // FIXME override hack for var when it's actually an fname
        return getFreeVars().isEmpty();
    }
}
