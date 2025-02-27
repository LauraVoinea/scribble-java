package org.scribble.ext.gt.core.model.efsm;

public class GTVState {

    private static int count = 1;

    public final int id;

    public GTVState() {
        this.id = GTVState.count++;
    }

    @Override
    public String toString() {
        return String.valueOf(this.id);
    }

    @Override
    public int hashCode() {
        int hash = 15443;
        hash = 31 * hash + this.id;
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof GTVState cast)) {
            return false;
        }
        return this.id == cast.id;
    }
}
