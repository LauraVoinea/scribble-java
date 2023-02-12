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
package org.scribble.core.model.global;

import org.scribble.core.model.ActionKind;
import org.scribble.core.model.endpoint.EFsm;
import org.scribble.core.model.global.actions.*;
import org.scribble.core.model.global.buffers.SBuffers;
import org.scribble.core.type.kind.Global;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.ProtoName;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;

import java.util.Map;

public interface SModelFactory {
    SGraphBuilderUtil SGraphBuilderUtil();

    // protected constructors (MState mutable)
    SState SState(SConfig config);

    SConfig SConfig(Map<Role, EFsm> state, SBuffers buffs);

    SGraph SGraph(ProtoName<Global> proto, Map<Integer, SState> states,
                  SState init);  // states: s.id -> s

    SModel SModel(SGraph g);

    // public constructors (subpackage, immutable)
    <A extends ActionKind> SSend<A> SSend(Role subj, Role obj, MsgId<?> mid, Payload pay);

    <A extends ActionKind> SRecv<A> SRecv(Role subj, Role obj, MsgId<?> mid, Payload pay);

    <A extends ActionKind> SReq<A> SReq(Role subj, Role obj, MsgId<?> mid, Payload pay);

    <A extends ActionKind> SAcc<A> SAcc(Role subj, Role obj, MsgId<?> mid, Payload pay);

    <A extends ActionKind> SDisconnect<A> SDisconnect(Role subj, Role obj);

    <A extends ActionKind> SClientWrap<A> SClientWrap(Role subj, Role obj);

    <A extends ActionKind> SServerWrap<A> SServerWrap(Role subj, Role obj);
}
