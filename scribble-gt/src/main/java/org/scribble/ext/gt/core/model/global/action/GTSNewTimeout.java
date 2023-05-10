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
package org.scribble.ext.gt.core.model.global.action;

import org.scribble.core.model.ActionKind;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.gt.core.model.local.GTEModelFactory;
import org.scribble.ext.gt.core.model.local.action.GTEAction;

public class GTSNewTimeout<A extends ActionKind> extends SAction<A> implements GTSAction {

    public final int c;
    public final int n;

    public GTSNewTimeout(int c, int n) {
        super(Role.EMPTY_ROLE, Role.EMPTY_ROLE, Op.EMPTY_OP, Payload.EMPTY_PAYLOAD);
        this.c = c;
        this.n = n;
    }

    /* ... */

    @Override
    public GTEAction project(GTEModelFactory mf) {
        return mf.DynamicGTENewTimeout(this.c, this.n);
    }

    /* ... */

    @Override
    public int getC() {
        return this.c;
    }

    @Override
    public int getN() {
        return this.n;
    }

    @Override
    public String toString() {
        return getCommSymbol() + " " + this.c + "," + this.n;
    }

    /* ... */

    @Override
    public int hashCode() {
        int hash = 33479;
        hash = 31 * hash + super.hashCode();
        hash = 31 * hash + this.c;
        hash = 31 * hash + this.n;
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof GTSNewTimeout)) {
            return false;
        }
        GTSNewTimeout<?> them = (GTSNewTimeout<?>) o;
        return super.equals(o)  // Does canEquals
                && this.c == them.c && this.n == them.n;
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTSNewTimeout;
    }

    @Override
    public String getCommSymbol() {
        return "\u03BD";
    }
}
