package org.scribble.ext.assrt.core.type.formal.global.action;

import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.global.AssrtFormalGType;
import org.scribble.ext.assrt.core.type.formula.*;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;

import java.util.List;
import java.util.stream.Collectors;

public class AssrtFormalGComm implements AssrtFormalGAction {

    public final Role src;
    public final Role dst;
    public final AssrtMsg msg;

    public AssrtFormalGComm(Role src, Role dst, AssrtMsg msg) {
        this.src = src;
        this.dst = dst;
        this.msg = msg;
    }

    public AssrtBFormula makeAssrtProgRhs() {
        if (this.msg.phantom != null || this.msg.phantAss != null) {  // should be null for core.type.formal?
            throw new RuntimeException("TODO phantoms? " + this);
        }
        if (this.msg.pay.size() == 0) {
            return this.msg.ass;
        }
        if (this.msg.pay.stream().anyMatch(x -> !x.data.toString().equals("int"))) {
            throw new RuntimeException("TODO non-int payload: " + this);
        }
        List<AssrtAVarFormula> vs = this.msg.pay.stream()
                .map(x -> (AssrtAVarFormula) AssrtFormulaFactory.AssrtIntVar(x.var.toString()))
                .collect(Collectors.toList());
        return AssrtFormulaFactory.AssrtExistsFormula(vs, this.msg.ass);
    }

    /* ... */

    @Override
    public String toString() {
        return this.src + "->" + this.dst + ":" + this.msg;
    }

    /* ... */

    @Override
    public int hashCode() {
        int hash = AssrtFormalGType.COMM_HASH;
        hash = 31 * hash + this.src.hashCode();
        hash = 31 * hash + this.dst.hashCode();
        hash = 31 * hash + this.msg.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof AssrtFormalGComm)) {
            return false;
        }
        AssrtFormalGComm them = (AssrtFormalGComm) o;
        return //them.canEquals(this) &&
                this.src.equals(them.src) && this.dst.equals(them.dst)
                        && this.msg.equals(them.msg);
    }

    //public abstract boolean canEquals(Object o);
}
