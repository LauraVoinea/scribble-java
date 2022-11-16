package org.scribble.ext.assrt.util;

public class Triple<T1, T2, T3> {
    public final T1 left;
    public final T2 middle;
    public final T3 right;

    public Triple(T1 t1, T2 t2, T3 t3) {
        this.left = t1;
        this.middle = t2;
        this.right = t3;
    }

    @Override
    public String toString() {
        return "(" + this.left + ", " + this.middle + ", " + this.right + ")";
    }

    @Override
    public int hashCode() {
        int hash = 10391;
        hash = 31 * hash + this.left.hashCode();
        hash = 31 * hash + this.right.hashCode();
        hash = 31 * hash + this.right.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
        {
            return true;
        }
        if (!(o instanceof Triple))
        {
            return false;
        }
        Triple<?, ?, ?> them = (Triple<?, ?, ?>) o;
        return this.left.equals(them.left) && this.middle.equals(them.middle)
                && this.right.equals(them.right);
    }
}
