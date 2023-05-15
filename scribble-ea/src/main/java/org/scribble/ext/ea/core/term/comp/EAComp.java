package org.scribble.ext.ea.core.term.comp;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.runtime.EAGlobalQueue;
import org.scribble.ext.ea.core.runtime.config.EACActor;
import org.scribble.ext.ea.core.term.expr.EAEFuncName;
import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.term.expr.EAERec;
import org.scribble.ext.ea.core.term.expr.EAEVar;
import org.scribble.ext.ea.core.term.expr.EAExpr;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.ConsoleColors;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

import javax.swing.text.DefaultEditorKit;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

// "Computation"
public interface EAComp extends EATerm {

    // Used for config state typing
    EALType infer(GammaState gamma);

    //Pair<EAVType, EALType> type(GammaState gamma, EALType pre);
    Either<Exception, Pair<Pair<EAVType, EALType>, Tree<String>>> type(GammaState gamma, EALType pre);

    // TODO factor out tag param
    default String toTypeJudgeString(GammaState gamma, EALType S, EAVType B, EALType T) {
        return gamma + " | " + S + " " + ConsoleColors.TRIANGLERIGHT + " "
                + this + ": " + B + " " + ConsoleColors.TRIANGLELEFT + " " + T;
    }

    // ->_M -- config independent M eval
    @Deprecated
    boolean canBeta();

    // ->_M -- config independent M eval
    //EAComp beta();  // !!! CHECKME deterministic
    Either<Exception, Pair<EAComp, Tree<String>>> beta();  // CHECKME deterministic?

    // TODO factor out tag param
    default String toBetaJudgeString(EAComp left, EAComp right) {
        return left + " " + ConsoleColors.RIGHTARROW + "_M " + right;
    }

    // basically using EAComp to "drive" eval steps in lieu of LTS action labels
    // Extract the (nested) "statically reducible part" CANDIDATE for config reduction -- e.g., send can only be a candidate (so app/let/etc don't check canBeta for foo -- EAPActiveThread.canStep checks canBeta on relevant foo, but could refactor some canBeta into getFoo)
    // ...deterministic(?)  // doesn't check canBeta, EAPActiveThread.canStep checks it as necessary
    EAComp getStepSubexprE();  // getFoo

    // Maybe deriv tree labels belong more to EACActor.reduce than to the "candidates" as here
    //EAComp configReduce();
    Either<Exception, Pair<EAComp, Tree<String>>> contextStepE();
    // ...separate above to contextStepE-Leaf(no Tree<String>) and Lift(Tree<String> for beta)


    /*default String toConfigRed1JudgeString(EACActor left, EACActor right) {
        return left ...
    }*/

    /* Aux */

    EAComp subs(@NotNull Map<EAEVar, EAExpr> m);

    EAComp fsubs(@NotNull Map<EAEFuncName, EAERec> m);

    // CHECKME needed?
    @Deprecated
    EAComp recon(@NotNull EAComp old, @NotNull EAComp neww);  // A subs for Expr (cf. Val)

    Set<EAEVar> getFreeVars();

    // FIXME separate "is ground" (cf. EAPSystem.reduce) from "is finished value" (cf. EAPLet.canBeta)
    default boolean isGround() {
        return getFreeVars().isEmpty();
    }

    // "isValue" for Comp
    default boolean isGroundValueReturn() {
        return false;
    }
}

