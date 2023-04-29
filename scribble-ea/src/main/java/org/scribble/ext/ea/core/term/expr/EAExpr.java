package org.scribble.ext.ea.core.term.expr;

import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

import java.util.Map;
import java.util.Set;

// "pure" exprs (no I/O)
public interface EAExpr extends EATerm {

    //HERE HERE make type/eval Optional/Either, deprecate canEval
    //...build scrib zip and make scala/kotlin?

    Either<Exception, Pair<EAVType, Tree<String>>> type(GammaState gamma);

    // var/funcname throw RuntimeException
    EAVType infer();  // for use on ground vals at config level

    default boolean canEval() {
        return false;
    }

    default EAExpr eval() {
        throw new RuntimeException("Stuck: " + this);
    }

    /* Aux */

    EAExpr subs(Map<EAEVar, EAExpr> m);

    EAExpr fsubs(Map<EAEFuncName, EAERec> m);  // Hack

    Set<EAEVar> getFreeVars();

    // !!! cf. "isValue" (cf. EAEBinOp)
    default boolean isGround() {  // FIXME override hack for var when it's actually an fname
        return getFreeVars().isEmpty();
    }

    // A ground value -- implies isGround, !canEval -- currently all except binop
    boolean isValue();
}
