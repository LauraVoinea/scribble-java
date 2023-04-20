package org.scribble.ext.ea.core.term.comp;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.*;
import org.scribble.ext.ea.core.term.expr.EAEFuncName;
import org.scribble.ext.ea.core.term.expr.EAERec;
import org.scribble.ext.ea.core.term.expr.EAExpr;
import org.scribble.ext.ea.core.term.expr.EAEVar;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.ConsoleColors;
import org.scribble.util.Pair;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

public class EAMLet implements EAComp {

    @NotNull
    public final EAEVar var;
    @NotNull
    public final EAVType varType;  // vars x have ValTypes A -- !!! added type annot
    //@NotNull public final EAPExpr init;  // !!! value?  not expr
    @NotNull
    public final EAComp init;
    @NotNull
    public final EAComp body;

    //public EAPLet(@NotNull EAPVar var, @NotNull EAPExpr init, @NotNull EAPExpr body) {
    public EAMLet(@NotNull EAEVar var, @NotNull EAVType varType,
                  @NotNull EAComp init, @NotNull EAComp body) {
        this.var = var;
        this.varType = varType;
        this.init = init;
        this.body = body;
    }

    @Override
    public Pair<EAVType, EALType> type(Gamma gamma, EALType pre) {
        Pair<EAVType, EALType> p1 = this.init.type(gamma, pre);
        if (!this.varType.equals(p1.left)) {
            throw new RuntimeException("Bad type annotation: "
                    + this.varType + ", " + p1.left);
        }
        LinkedHashMap<EAName, EAVType> tmp = new LinkedHashMap<>(gamma.map);
        tmp.put(this.var, p1.left);
        Gamma gamma1 = new Gamma(tmp, new LinkedHashMap<>(gamma.fmap), gamma.svar, gamma.svarType);
        return this.body.type(gamma1, p1.right);
    }

    @Override
    public EALType infer(Gamma gamma) {
        EALType i = this.init.infer(gamma);
        LinkedHashMap<EAName, EAVType> tmp = new LinkedHashMap<>(gamma.map);
        tmp.put(this.var, this.varType);
        Gamma gamma1 = new Gamma(tmp, new LinkedHashMap<>(gamma.fmap), gamma.svar, gamma.svarType);
        EALType b = this.body.infer(gamma1);
        return i.concat(b);
    }

    @Override
    public boolean canBeta() {
        return this.init.canBeta() || this.init.isGroundValueReturn();
    }

    @Override
    public EAComp beta() {
        if (!canBeta()) {
            throw new RuntimeException("Stuck: " + this);
        }
        if (this.init.canBeta()) {
            return EATermFactory.factory.let(this.var, this.varType, this.init.beta(), this.body);
        } else {  // this.init instanceof EAPReturn && this.init.isGround()
            return this.body.subs(Map.of(this.var, ((EAMReturn) this.init).val));
        }
    }

    // foo (getConfigRedexCandidate) return corresponds with beta "subject"
    @Override
    public EAComp getConfigRedexCandidate() {
        /*if (this.init instanceof EAMReturn //&& ((EAPReturn) this.init).val.isGround()
                && !((EAMReturn) this.init).val.canEval()) {*/
        if (this.init.isGroundValueReturn()) {
            return this;
        } else {
            return this.init.getConfigRedexCandidate();
        }
    }

    @Override
    public EAComp configReduce() {  // Not beta because, e.g., send in init cannot beta (must foo)
        /*if (this.init instanceof EAMReturn && //this.init.isGround()) {
                !this.init.canBeta()) {*/
        if (this.init.isGroundValueReturn()) {
            return this.body.subs(Map.of(this.var, ((EAMReturn) this.init).val));
        } else {
            return EATermFactory.factory.let(this.var, this.varType, this.init.configReduce(), this.body);
        }
    }

    /* Aux */

    @Override
    public EAMLet subs(@NotNull Map<EAEVar, EAExpr> m) {
        EAComp init1 = this.init.subs(m);
        Map<EAEVar, EAExpr> m1 = new HashMap<>(m);
        m1.remove(this.var);
        EAComp body1 = body.subs(m1);
        return EATermFactory.factory.let(this.var, this.varType, init1, body1);
    }

    @Override
    public EAMLet fsubs(@NotNull Map<EAEFuncName, EAERec> m) {
        EAComp init1 = this.init.fsubs(m);
        EAComp body1 = body.fsubs(m);
        return EATermFactory.factory.let(this.var, this.varType, init1, body1);
    }

    @Override
    public EAComp recon(@NotNull EAComp old, EAComp neww) {
        EAComp init1 = this.init.recon(old, neww);
        return EATermFactory.factory.let(this.var, this.varType, init1, this.body);  // !!! CHECKME: body unchanged
    }

    @Override
    public Set<EAEVar> getFreeVars() {
        Set<EAEVar> res = this.init.getFreeVars();
        res.addAll(this.body.getFreeVars());
        res.remove(this.var);
        return res;
    }

    /*@Override
    public boolean isGround() {
        //return this.init.isGround();  // !!! bad naming
        return getFreeVars().isEmpty();
    }*/

    @Override
    public String toString() {
        return "let " + this.var + " "
                //+ ConsoleColors.BLACK_UNDERLINED + ":" + this.varType + ConsoleColors.RESET
                + ConsoleColors.toAnnotString(":" + this.varType)
                + " <= " + this.init + " in " + this.body;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAMLet them = (EAMLet) o;
        return them.canEquals(this)
                && this.var.equals(them.var)
                && this.varType.equals(them.varType)
                && this.init.equals(them.init)
                && this.body.equals(them.body);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAMLet;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.LET;
        hash = 31 * hash + this.var.hashCode();
        hash = 31 * hash + this.varType.hashCode();
        hash = 31 * hash + this.init.hashCode();
        hash = 31 * hash + this.body.hashCode();
        return hash;
    }
}
