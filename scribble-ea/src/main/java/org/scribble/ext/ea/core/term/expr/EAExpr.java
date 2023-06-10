package org.scribble.ext.ea.core.term.expr;

import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.term.comp.EAComp;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.ConsoleColors;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

import java.util.Map;
import java.util.Set;

// TODO rename EAVAl
// "pure" exprs (no I/O)
public interface EAExpr extends EATerm {

    //HERE HERE make type/eval Optional/Either, deprecate canEval
    //...build scrib zip and make scala/kotlin?

    default Exception newStuck() {
        return new Exception("Stuck: " + this);
    }

    Either<Exception, Pair<EAVType, Tree<String>>> type(GammaState gamma);

    default String toTypeJudgeString(GammaState gamma, EAVType A) {
        return gamma + " " + ConsoleColors.VDASH + " " + this + " : " + A;
    }

    // var/funcname throw RuntimeException
    EAVType infer();  // for use on ground vals at config level

    @Deprecated  // CHECKME un-deprec? use eval?
    default boolean canEval() {
        return false;
    }

    Either<Exception, Pair<EAExpr, Tree<String>>> eval();

    /* Aux */

    EAExpr subs(Map<EAEVar, EAExpr> m);

    EAExpr fsubs(Map<EAEFuncName, EAERec> m);  // Hack

    Set<EAEVar> getFreeVars();

    // CHECKME mostly redundant? -- just analogy to EAComp.isGround
    // ground expression (potentially eligible for reduction step) !!! cf. "isValue" (cf. EAEBinOp)
    default boolean isGround() {  // FIXME override hack for var when it's actually an fname
        return getFreeVars().isEmpty();
    }

    // A ground value (reduction finished) -- implies isGround, !canEval -- currently all except binop
    boolean isValue();
}
