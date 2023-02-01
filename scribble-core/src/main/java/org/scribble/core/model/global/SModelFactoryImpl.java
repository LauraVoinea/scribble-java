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
import org.scribble.core.model.ModelFactory;
import org.scribble.core.model.ModelFactoryBase;
import org.scribble.core.model.endpoint.EFsm;
import org.scribble.core.model.global.actions.*;
import org.scribble.core.model.global.buffers.SBuffers;
import org.scribble.core.type.kind.Global;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.ProtoName;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;

import java.util.Map;

// Separate E/SModelFactories fits protected E/SState constructor pattern
public class SModelFactoryImpl extends ModelFactoryBase implements SModelFactory {

    public SModelFactoryImpl(ModelFactory mf) {
        super(mf);
    }

    @Override
    public SGraphBuilderUtil SGraphBuilderUtil() {
        return new SGraphBuilderUtil(this.mf);
    }

    @Override
    public SState SState(SConfig config) {
        return new SState(config);
    }

    // states: s.id -> s
    @Override
    public SGraph SGraph(ProtoName<Global> proto, Map<Integer, SState> states,
                         SState init) {
        return new SGraph(proto, states, init);
    }

    @Override
    public SConfig SConfig(Map<Role, EFsm> state, SBuffers buffs) {
        return new SConfig(this.mf, state, buffs);
    }

    @Override
    public SModel SModel(SGraph g) {
        return new SModel(g);
    }

    @Override
    public <A extends ActionKind> SSend<A> SSend(Role subj, Role obj, MsgId<?> mid, Payload pay) {
        return new SSend<>(subj, obj, mid, pay);
    }

    @Override
    public <A extends ActionKind> SRecv<A> SRecv(Role subj, Role obj, MsgId<?> mid, Payload pay) {
        return new SRecv<>(subj, obj, mid, pay);
    }

    @Override
    public <A extends ActionKind> SReq<A> SReq(Role subj, Role obj, MsgId<?> mid, Payload pay) {
        return new SReq<>(subj, obj, mid, pay);
    }

    @Override
    public <A extends ActionKind> SAcc<A> SAcc(Role subj, Role obj, MsgId<?> mid, Payload pay) {
        return new SAcc<>(subj, obj, mid, pay);
    }

    @Override
    public <A extends ActionKind> SDisconnect<A> SDisconnect(Role subj, Role obj) {
        return new SDisconnect<>(subj, obj);
    }

    @Override
    public <A extends ActionKind> SClientWrap<A> SClientWrap(Role subj, Role obj) {
        return new SClientWrap<>(subj, obj);
    }

    @Override
    public <A extends ActionKind> SServerWrap<A> SServerWrap(Role subj, Role obj) {
        return new SServerWrap<>(subj, obj);
    }
}
