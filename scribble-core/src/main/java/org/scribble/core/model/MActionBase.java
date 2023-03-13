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
package org.scribble.core.model;

import org.scribble.core.type.kind.ProtoKind;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.util.RuntimeScribException;

public abstract class MActionBase<K extends ProtoKind, A extends ActionKind>
        implements MAction<K, A> {

    public static final int DYNAMIC_ID = -1;

    private static int count = 1;  // >= 1

    public final int id;  // Was using for trace enumeration, but breaks isAcceptable -- but would be better for non-det models?

    public final Role obj;
    public final MsgId<?> mid;
    public final Payload payload;  // Payload.EMPTY_PAYLOAD for SigName mid

    protected MActionBase(int id, Role obj, MsgId<?> mid, Payload payload) {
        this.id = id;
        this.obj = obj;
        this.mid = mid;
        this.payload = payload;
    }

    @Override
    public int getId() {
        return this.id;
    }

    public static int nextCount() {
        if (MActionBase.count == Integer.MAX_VALUE) {
            throw new RuntimeScribException("[FIXME] Max actions reached.");
        }
        return MActionBase.count++;
    }

    @Override
    public Role getObject() {
        return this.obj;
    }

    @Override
    public MsgId<?> getMid() {
        return this.mid;
    }

    @Override
    public Payload getPayload() {
        return this.payload;
    }

    @Override
    public String toString() {
        return this.obj + getCommSymbol() + this.mid + this.payload;
    }

    // Used by toAut
    public String toStringWithMsgIdHack() {
        String m = this.mid.isSigName() ? "^" + this.mid : this.mid.toString();  // HACK
        return this.obj + getCommSymbol() + m + this.payload;
    }

    public abstract String getCommSymbol();

    @Override
    public int hashCode() {
        int hash = 919;
        hash = 31 * hash + this.id;
        hash = 31 * hash + this.obj.hashCode();
        hash = 31 * hash + this.mid.hashCode();
        hash = 31 * hash + this.payload.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof MActionBase)) {
            return false;
        }
        MActionBase<?, ?> them = (MActionBase<?, ?>) o;  // Refactor as "compatible"
        return them.canEquals(this)
                && this.id == them.id && this.obj.equals(them.obj)
                && this.mid.equals(them.mid) && this.payload.equals(them.payload);
    }

    public abstract boolean canEquals(Object o);
}
