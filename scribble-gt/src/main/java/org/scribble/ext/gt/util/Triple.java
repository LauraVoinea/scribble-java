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

public class Triple<T1, T2, T3> {
    public final T1 left;
    public final T2 mid;
    public final T3 right;

    public Triple(T1 t1, T2 t2, T3 t3) {
        this.left = t1;
        this.mid = t2;
        this.right = t3;
    }

    public static <T1, T2, T3> Triple<T1, T2, T3> of(T1 t1, T2 t2, T3 t3) {
        return new Triple(t1, t2, t3);
    }

    @Override
    public String toString() {
        return "(" + this.left + ", " + this.mid + ", " + this.right + ")";
    }

    @Override
    public int hashCode() {
        int hash = 11;
        hash = 31 * hash + this.left.hashCode();
        hash = 31 * hash + this.mid.hashCode();
        hash = 31 * hash + this.right.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof Triple)) {
            return false;
        }
        Triple<?, ?, ?> them = (Triple<?, ?, ?>) o;
        return this.left.equals(them.left) && this.mid.equals(them.mid)
                && this.right.equals(them.right);
    }
}
