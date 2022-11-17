package org.scribble.ext.assrt.util;

public class Quadple<T1, T2, T3, T4> {
    public final T1 fst;
    public final T2 snd;
    public final T3 thd;
    public final T4 fth;

    public Quadple(T1 t1, T2 t2, T3 t3, T4 t4) {
        this.fst = t1;
        this.snd = t2;
        this.thd = t3;
        this.fth = t4;
    }

    @Override
    public String toString() {
        return "(" + this.fst + ", " + this.snd + ", " + this.thd + ", " + this.fth + ")";
    }

    @Override
    public int hashCode() {
        int hash = 10399;
        hash = 31 * hash + this.fst.hashCode();
        hash = 31 * hash + this.thd.hashCode();
        hash = 31 * hash + this.thd.hashCode();
        hash = 31 * hash + this.fth.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
        {
            return true;
        }
        if (!(o instanceof Quadple))
        {
            return false;
        }
        Quadple<?, ?, ?, ?> them = (Quadple<?, ?, ?, ?>) o;
        return this.fst.equals(them.fst) && this.snd.equals(them.snd)
                && this.thd.equals(them.thd) && this.fth.equals(them.fth);
    }
}
