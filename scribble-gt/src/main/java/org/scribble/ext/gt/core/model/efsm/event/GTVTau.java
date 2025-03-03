package org.scribble.ext.gt.core.model.efsm.event;

import org.scribble.core.type.name.Op;
import org.scribble.ext.gt.util.ConsoleColors;

public class GTVTau implements GTVEvent {

    public final Op op;  // !!! pay?

    public GTVTau(Op op) {
        this.op = op;
    }

    @Override
    public Kind getKind() {
        return Kind.INTERNAL;
    }

    @Override
    public String toString() {
        return ConsoleColors.TAU + "_" + this.op;
    }

    @Override
    public int hashCode() {
        int hash = 15451;
        hash = 31 * hash + this.op.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof GTVTau cast)) {
            return false;
        }
        return this.op.equals(cast.op);
    }
}
