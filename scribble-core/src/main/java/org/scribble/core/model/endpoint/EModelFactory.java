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

import org.scribble.core.model.ActionKind;
import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.StaticActionKind;
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

public interface EModelFactory {

    EGraphBuilderUtil EGraphBuilderUtil();

    // protected constructors (MState mutable)
    EState EState(Set<RecVar> labs);
    //EFsm new EFsm(...)

    // public constructors (subpackage, immutable)
    ESend<StaticActionKind> StaticESend(Role peer, MsgId<?> mid, Payload pay);

    ERecv<StaticActionKind> StaticERecv(Role peer, MsgId<?> mid, Payload pay);

    EReq<StaticActionKind> StaticEReq(Role peer, MsgId<?> mid, Payload pay);

    EAcc<StaticActionKind> StaticEAcc(Role peer, MsgId<?> mid, Payload pay);

    EDisconnect<StaticActionKind> StaticEDisconnect(Role peer);

    EClientWrap<StaticActionKind> StaticEClientWrap(Role peer);

    EServerWrap<StaticActionKind> StaticEServerWrap(Role peer);

    ESend<DynamicActionKind> DynamicESend(Role peer, MsgId<?> mid, Payload pay);

    ERecv<DynamicActionKind> DynamicERecv(Role peer, MsgId<?> mid, Payload pay);

    EReq<DynamicActionKind> DynamicEReq(Role peer, MsgId<?> mid, Payload pay);

    EAcc<DynamicActionKind> DynamicEAcc(Role peer, MsgId<?> mid, Payload pay);

    EDisconnect<DynamicActionKind> DynamicEDisconnect(Role peer);

    EClientWrap<DynamicActionKind> DynamicEClientWrap(Role peer);

    EServerWrap<DynamicActionKind> DynamicEServerWrap(Role peer);
}
