package org.scribble.ext.ea.core.term.comp;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.term.EATermFactory;
import org.scribble.ext.ea.core.term.expr.EAEFuncName;
import org.scribble.ext.ea.core.term.expr.EAERec;
import org.scribble.ext.ea.core.term.expr.EAEVar;
import org.scribble.ext.ea.core.term.expr.EAExpr;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.EALEndType;
import org.scribble.ext.ea.core.type.session.local.EALOutType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.session.local.EALTypeFactory;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.core.type.value.EAVUnitType;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class EAMSend implements EAComp {

    @NotNull
    public final Role dst;
    @NotNull
    public final Op op;
    @NotNull
    public final EAExpr val;  // value, not expr

    public EAMSend(@NotNull Role dst, @NotNull Op op, @NotNull EAExpr val) {
        this.dst = dst;
        this.op = op;
        this.val = val;
    }

    @Override
    //public Pair<EAVType, EALType> type(GammaState gamma, EALType pre) {
    public Either<Exception, Pair<Pair<EAVType, EALType>, Tree<String>>> type(GammaState gamma, EALType pre) {
        if (!(pre instanceof EALOutType)) {
            //throw new RuntimeException("Expected out type: " + pre + ", " + this);
            return Either.left(new Exception("Expected out type: " + pre + ", " + this));
        }
        EALOutType cast = (EALOutType) pre;
        // XXX FIXME self comm
        if (!cast.peer.equals(this.dst)) {
            //throw new RuntimeException("Incompatible peer: " + pre + ", " + this);
            return Either.left(new Exception("Incompatible peer: " + pre + ", " + this));
        }
        if (!cast.cases.containsKey(this.op)) {
            //throw new RuntimeException("Invalid op: " + pre + ", " + this);
            return Either.left(new Exception("Invalid op: " + pre + ", " + this));
        }
        //EAVType valType = this.val.type(gamma);
        Either<Exception, Pair<EAVType, Tree<String>>> t = this.val.type(gamma);
        if (t.isLeft()) {
            return Either.left(t.getLeft().get());
        }
        Pair<EAVType, Tree<String>> pp = t.getRight().get();
        EAVType valType = pp.left;
        Pair<EAVType, EALType> p = cast.cases.get(this.op);
        if (!valType.equals(p.left)) {
            //throw new RuntimeException("Incompatible value type: " + valType + ", " + p.left);
            return Either.left(new Exception("Incompatible value type: " + valType + ", " + p.left));
        }
        //return new Pair<>(EATypeFactory.factory.val.unit(), p.right);
        EAVUnitType B = EATypeFactory.factory.val.unit();
        return Either.right(Pair.of(
                Pair.of(B, p.right),
                new Tree<>("[T-Send] " + toJudgementString(gamma, pre, B, p.right),
                        List.of(pp.right))
        ));
    }

    @Override
    public EALOutType infer(GammaState gamma) {
        //EAVType t = this.val.type(gamma);
        Either<Exception, Pair<EAVType, Tree<String>>> r = this.val.type(gamma);
        if (r.isLeft()) {
            throw new RuntimeException(r.getLeft().get());  // FIXME infer Either
        }
        Pair<EAVType, Tree<String>> p = r.getRight().get();
        EAVType t = p.left;
        LinkedHashMap<Op, Pair<EAVType, EALType>> cases = new LinkedHashMap<>();
        cases.put(this.op, new Pair<>(t, EALEndType.END));  // !!! (potential) placeholder END
        return EALTypeFactory.factory.out(this.dst, cases);  // ...p.right discarded
    }

    @Override
    public boolean canBeta() {
        //return true;
        return false;
    }

    @Override
    public Either<Exception, Pair<EAComp, Tree<String>>> beta() {
        return Either.left(new Exception("Stuck: " + this));
    }

    /*@Override
    public boolean canFoo() {
        return false;
    }*/

    @Override
    public EAComp getConfigRedexCandidate() {
        return this;
    }

    @Override
    public Either<Exception, Pair<EAComp, Tree<String>>> configReduce() {
        return Either.right(Pair.of(
                EATermFactory.factory.returnn(EATermFactory.factory.unit()),
                new Tree<>("[E-Comm]")  // XXX !!! FIXME should be in EACActor, not here -- e.g., gets nested under E-Lift-Let
        ));
    }

    /* Aux */

    @Override
    public Set<EAEVar> getFreeVars() {
        return this.val.getFreeVars();
    }

    /*@Override
    public boolean isGround() {
        return this.val.isGround();
    }*/

    @Override
    public EAMSend subs(@NotNull Map<EAEVar, EAExpr> m) {
        EAExpr val1 = this.val.subs(m);
        return EATermFactory.factory.send(this.dst, this.op, val1);
    }

    @Override
    public EAMSend fsubs(@NotNull Map<EAEFuncName, EAERec> m) {
        EAExpr val1 = this.val.fsubs(m);
        return EATermFactory.factory.send(this.dst, this.op, val1);
    }

    @Override
    public EAComp recon(@NotNull EAComp old, EAComp neww) {
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
        EAMSend them = (EAMSend) o;
        return them.canEquals(this)
                && this.dst.equals(them.dst)
                && this.op.equals(them.op)
                && this.val.equals(them.val);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAMSend;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.SEND;
        hash = 31 * hash + this.dst.hashCode();
        hash = 31 * hash + this.op.hashCode();
        hash = 31 * hash + this.val.hashCode();
        return hash;
    }
}
