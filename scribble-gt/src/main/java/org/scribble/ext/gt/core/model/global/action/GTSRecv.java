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
import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.model.global.actions.SRecv;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.gt.core.model.local.GTEModelFactory;
import org.scribble.ext.gt.core.model.local.action.GTEAction;
import org.scribble.ext.gt.core.model.local.action.GTERecv;

public class GTSRecv<A extends ActionKind> extends SRecv<A> implements GTSAction {

    public final int c;
    public final int n;

    public GTSRecv(Role subj, Role obj, MsgId<?> mid, Payload pay, int c, int n) {
        super(subj, obj, mid, pay);
        this.c = c;
        this.n = n;
    }

    /* ... */

    @Override
    public GTERecv<DynamicActionKind> project(GTEModelFactory mf) {
        return mf.DynamicGTERecv(this.obj, this.mid, this.payload, this.c, this.n);
    }

    @Override
    public SAction<DynamicActionKind> toDynamic() {
        return GTSAction.super.toDynamic();
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
        return this.subj + getCommSymbol() + this.obj + "(" + this.mid
                + ", " + this.payload + ",  " + this.c + ", " + this.n + ")";
    }

    /* ... */

    @Override
    public int hashCode() {
        int hash = 33493;
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
        if (!(o instanceof GTSRecv)) {
            return false;
        }
        GTSRecv<?> them = (GTSRecv<?>) o;
        return super.equals(o)  // Does canEquals
                && this.c == them.c && this.n == them.n;
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTSRecv;
    }
}
