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
package org.scribble.core.model.endpoint;

import java.util.Set;

import org.scribble.core.model.*;
import org.scribble.core.model.endpoint.actions.EAcc;
import org.scribble.core.model.endpoint.actions.EClientWrap;
import org.scribble.core.model.endpoint.actions.EDisconnect;
import org.scribble.core.model.endpoint.actions.ERecv;
import org.scribble.core.model.endpoint.actions.EReq;
import org.scribble.core.model.endpoint.actions.ESend;
import org.scribble.core.model.endpoint.actions.EServerWrap;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;

// Separate E/SModelFactories fits protected E/SState constructor pattern
public class EModelFactoryImpl extends ModelFactoryBase implements EModelFactory {

    public EModelFactoryImpl(ModelFactory mf) {
        super(mf);
    }

    @Override
    public EGraphBuilderUtil EGraphBuilderUtil() {
        return new EGraphBuilderUtil(this.mf);
    }

    @Override
    public EState EState(Set<RecVar> labs) {
        return new EState(labs);
    }

    @Override
    public ESend<StaticActionKind> StaticESend(Role peer, MsgId<?> mid, Payload pay) {
        return new ESend<>(1, this.mf, peer, mid, pay);
    }

    @Override
    public ERecv<StaticActionKind> StaticERecv(Role peer, MsgId<?> mid, Payload pay) {
        return new ERecv<>(1, this.mf, peer, mid, pay);
    }

    @Override
    public EReq<StaticActionKind> StaticEReq(Role peer, MsgId<?> mid, Payload pay) {
        return new EReq<>(1, this.mf, peer, mid, pay);
    }

    @Override
    public EAcc<StaticActionKind> StaticEAcc(Role peer, MsgId<?> mid, Payload pay) {
        return new EAcc<>(1, this.mf, peer, mid, pay);
    }

    @Override
    public EDisconnect<StaticActionKind> StaticEDisconnect(Role peer) {
        return new EDisconnect<>(1, this.mf, peer);
    }

    @Override
    public EClientWrap<StaticActionKind> StaticEClientWrap(Role peer) {
        return new EClientWrap<>(1, this.mf, peer);
    }

    @Override
    public EServerWrap<StaticActionKind> StaticEServerWrap(Role peer) {
        return new EServerWrap<>(1, this.mf, peer);
    }

    @Override
    public ESend<DynamicActionKind> DynamicESend(Role peer, MsgId<?> mid, Payload pay) {
        return new ESend<>(MActionBase.DYNAMIC_ID, this.mf, peer, mid, pay);
    }

    @Override
    public ERecv<DynamicActionKind> DynamicERecv(Role peer, MsgId<?> mid, Payload pay) {
        return new ERecv<>(MActionBase.DYNAMIC_ID, this.mf, peer, mid, pay);
    }

    @Override
    public EReq<DynamicActionKind> DynamicEReq(Role peer, MsgId<?> mid, Payload pay) {
        return new EReq<>(MActionBase.DYNAMIC_ID, this.mf, peer, mid, pay);
    }

    @Override
    public EAcc<DynamicActionKind> DynamicEAcc(Role peer, MsgId<?> mid, Payload pay) {
        return new EAcc<>(MActionBase.DYNAMIC_ID, this.mf, peer, mid, pay);
    }

    @Override
    public EDisconnect<DynamicActionKind> DynamicEDisconnect(Role peer) {
        return new EDisconnect<>(MActionBase.DYNAMIC_ID, this.mf, peer);
    }

    @Override
    public EClientWrap<DynamicActionKind> DynamicEClientWrap(Role peer) {
        return new EClientWrap<>(MActionBase.DYNAMIC_ID, this.mf, peer);
    }

    @Override
    public EServerWrap<DynamicActionKind> DynamicEServerWrap(Role peer) {
        return new EServerWrap<>(MActionBase.DYNAMIC_ID, this.mf, peer);
    }
}
