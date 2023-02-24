package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.EALEndType;
import org.scribble.ext.ea.core.type.session.local.EALOutType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.session.local.EALTypeFactory;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.util.EAPPair;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

public class EAPSend implements EAPExpr {

    @NotNull public final Role dst;
    @NotNull public final Op op;
    @NotNull public final EAPVal val;  // value, not expr

    public EAPSend(@NotNull Role dst, @NotNull Op op, @NotNull EAPVal val) {
        this.dst = dst;
        this.op = op;
        this.val = val;
    }

    @Override
    public EAPPair<EAValType, EALType> type(Gamma gamma, EALType pre) {
        if (!(pre instanceof EALOutType)) {
            throw new RuntimeException("Expected out type: " + pre + ", " + this);
        }
        EALOutType cast = (EALOutType) pre;
        if (!cast.peer.equals(this.dst)) {
            throw new RuntimeException("Incompatible peer: " + pre + ", " + this);
        }
        if (!cast.cases.containsKey(this.op)) {
            throw new RuntimeException("Invalid op: " + pre + ", " + this);
        }
        EAValType valType = this.val.type(gamma);
        Pair<EAValType, EALType> p = cast.cases.get(this.op);
        if (!valType.equals(p.left)) {
            throw new RuntimeException("Incompatible value type: " + valType + ", " + p.left);
        }
        return new EAPPair<>(EATypeFactory.factory.val.unit(), p.right);
    }

    @Override
    public EALOutType infer(Gamma gamma) {
        EAValType t = this.val.type(gamma);
        LinkedHashMap<Op, EAPPair<EAValType, EALType>> cases = new LinkedHashMap<>();
        cases.put(this.op, new EAPPair<>(t, EALEndType.END));  // !!! (potential) placeholder END
        return EALTypeFactory.factory.out(this.dst, cases);
    }

    @Override
    public boolean canBeta() {
        //return true;
        return false;
    }

    @Override
    public EAPExpr beta() {
        throw new RuntimeException("Stuck: " + this);
        //return EAPFactory.factory.returnn(EAPFactory.factory.unit());
    }

    /* Aux */

    @Override
    public Set<EAPVar> getFreeVars() {
        return this.val.getFreeVars();
    }

    @Override
    public boolean isGround() {
        return this.val.isGround();
    }

    /*@Override
    public boolean canFoo() {
        return false;
    }*/

    @Override
    public EAPExpr getFoo() {
        return this;
    }

    @Override
    public EAPExpr foo() {
        return EAPFactory.factory.returnn(EAPFactory.factory.unit());
    }

    @Override
    public EAPSend subs(@NotNull Map<EAPVar, EAPVal> m) {
        EAPVal val1 = this.val.subs(m);
        return EAPFactory.factory.send(this.dst, this.op, val1);
    }

    @Override
    public EAPSend fsubs(@NotNull Map<EAPFuncName, EAPRec> m) {
        EAPVal val1 = this.val.fsubs(m);
        return EAPFactory.factory.send(this.dst, this.op, val1);
    }

    @Override
    public EAPExpr recon(@NotNull EAPExpr old, EAPExpr neww) {
        return this.equals(old) ? neww : this;
    }

    @Override
    public String toString() {
        return this.dst + "!" + this.op + "(" + this.val + ")";
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPSend them = (EAPSend) o;
        return them.canEquals(this)
                && this.dst.equals(them.dst)
                && this.op.equals(them.op)
                && this.val.equals(them.val);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPSend;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.SEND;
        hash = 31 * hash + this.dst.hashCode();
        hash = 31 * hash + this.op.hashCode();
        hash = 31 * hash + this.val.hashCode();
        return hash;
    }
}
