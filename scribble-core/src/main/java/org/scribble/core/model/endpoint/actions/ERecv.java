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
package org.scribble.core.model.endpoint.actions;

import org.scribble.core.model.ActionKind;
import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.ModelFactory;
import org.scribble.core.model.StaticActionKind;
import org.scribble.core.model.global.actions.SRecv;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;

public class ERecv<A extends ActionKind> extends EAction<A> {

    public ERecv(ModelFactory ef, Role peer, MsgId<?> mid, Payload pay) {
        super(ef, peer, mid, pay);
    }

    @Override
    public ERecv<DynamicActionKind> toDynamic() {
        return this.mf.local.ERecv(this.peer, this.mid, this.payload);
    }

    @Override
    public ESend<DynamicActionKind> toDynamicDual(Role self) {
        return this.mf.local.ESend(self, this.mid, this.payload);
    }

    @Override
    public SRecv<StaticActionKind> toStaticGlobal(Role self) {
        return this.mf.global.SRecv(self, this.peer, this.mid, this.payload);

    }

    @Override
    public int hashCode() {
        int hash = 947;
        hash = 31 * hash + super.hashCode();
        return hash;
    }

    @Override
    public boolean isReceive() {
        return true;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof ERecv)) {
            return false;
        }
        return super.equals(o);  // Does canEquals
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof ERecv;
    }

    @Override
    public String getCommSymbol() {
        return "?";
    }
}
