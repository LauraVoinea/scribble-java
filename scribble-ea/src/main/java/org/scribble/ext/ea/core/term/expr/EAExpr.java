package org.scribble.ext.ea.core.term.expr;

import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EAVType;

import java.util.Map;
import java.util.Set;

// "pure" exprs (no I/O)
public interface EAExpr extends EATerm {
    //EAPExpr {  !!! values should be exprs?

    EAVType infer();  // not intended to work for single var -- for use on ground vals at config level

    default boolean canEval() {
        return false;
    }

    default EAExpr eval() {
        throw new RuntimeException("Stuck: " + this);
    }

    EAVType type(Gamma gamma);

    EAExpr subs(Map<EAEVar, EAExpr> m);

    EAExpr fsubs(Map<EAEFuncName, EAERec> m);

    Set<EAEVar> getFreeVars();

    // !!! cf. "isValue" (cf. EAEBinOp)
    default boolean isGround() {  // FIXME override hack for var when it's actually an fname
        return getFreeVars().isEmpty();
    }

    // A ground value -- implies isGround, !canEval
    boolean isValue();
}
