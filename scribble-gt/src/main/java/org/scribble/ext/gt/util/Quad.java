/*
 * Copyright 2008 The Scribble Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scribble.ext.gt.util;

public class Quad<T1, T2, T3, T4> {
   
    public final T1 fst;
    public final T2 snd;
    public final T3 thrd;
    public final T4 frth;

    public Quad(T1 t1, T2 t2, T3 t3, T4 t4) {
        this.fst = t1;
        this.snd = t2;
        this.thrd = t3;
        this.frth = t4;
    }

    public static <T1, T2, T3, T4> Quad<T1, T2, T3, T4> of(
            T1 t1, T2 t2, T3 t3, T4 t4) {
        return new Quad(t1, t2, t3, t4);
    }

    @Override
    public String toString() {
        return "(" + this.fst + ", " + this.snd + ", " + this.thrd + ", " + this.frth + ")";
    }

    @Override
    public int hashCode() {
        int hash = 11;
        hash = 31 * hash + this.fst.hashCode();
        hash = 31 * hash + this.snd.hashCode();
        hash = 31 * hash + this.thrd.hashCode();
        hash = 31 * hash + this.frth.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof Quad)) {
            return false;
        }
        Quad<?, ?, ?, ?> them = (Quad<?, ?, ?, ?>) o;
        return this.fst.equals(them.fst) && this.snd.equals(them.snd)
                && this.thrd.equals(them.thrd) && this.frth.equals(them.frth);
    }
}
