package org.scribble.ext.assrt.core.model.formal.endpoint;

import org.scribble.core.model.MState;
import org.scribble.ext.assrt.core.model.endpoint.AssrtEState;

public class RCAState {
    public static int counter = 0;

    public final int id;

    protected RCAState(int id) {
        this.id = id;
    }

    public static RCAState fresh() {
        return new RCAState(RCAState.counter++);
    }

    @Override
    public String toString() {
        return Integer.toString(this.id);
    }

    @Override
    public int hashCode()
    {
        int hash = 23063;
        hash = 31 * hash + this.id;
        return hash;
    }

    @Override
    public boolean equals(Object o)
    {
        if (this == o)
        {
            return true;
        }
        if (!(o instanceof RCAState))
        {
            return false;
        }
        RCAState them = (RCAState) o;
        return this.id == them.id;
    }

    /*@Override
    protected boolean canEquals(MState<?, ?, ?, ?> s)
    {
        return s instanceof AssrtEState;
    }*/
}
