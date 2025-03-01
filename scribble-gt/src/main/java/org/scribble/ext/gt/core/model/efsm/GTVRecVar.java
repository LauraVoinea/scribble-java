package org.scribble.ext.gt.core.model.efsm;

import org.scribble.core.type.name.RecVar;

public class GTVRecVar extends GTVState {

    public final RecVar recvar;

    public GTVRecVar(int c, RecVar recvar) {
        super(c);
        this.recvar = recvar;
    }

    @Override
    public String toString() {
        return super.toString() + ": " + this.recvar.toString();
    }

    @Override
    public int hashCode() {
        int hash = 15901;
        hash = 31 * hash + super.hashCode();
        hash = 31 * hash + this.recvar.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof GTVRecVar cast)) {
            return false;
        }
        return super.equals(cast) && this.recvar == cast.recvar;
    }
}
