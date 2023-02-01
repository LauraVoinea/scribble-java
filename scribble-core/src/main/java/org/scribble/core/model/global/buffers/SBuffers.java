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
package org.scribble.core.model.global.buffers;

import java.util.*;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.endpoint.actions.EAcc;
import org.scribble.core.model.endpoint.actions.EClientWrap;
import org.scribble.core.model.endpoint.actions.EDisconnect;
import org.scribble.core.model.endpoint.actions.ERecv;
import org.scribble.core.model.endpoint.actions.EReq;
import org.scribble.core.model.endpoint.actions.ESend;
import org.scribble.core.model.endpoint.actions.EServerWrap;
import org.scribble.core.type.name.Role;

// Immutable -- send/receive/etc return updated copies
public interface SBuffers {

    // `?` means buffer semantics not constrainted by FSM transition ids -- buffer actions independent of transitions
    boolean canSend(Role self, ESend<?> a);

    boolean canReceive(Role self, ERecv<?> a);

    // N.B. "sync" action but only considers the self side, i.e., to actually fire, must also explicitly check canAccept
    boolean canRequest(Role self, EReq<?> c);

    // N.B. "sync" action but only considers the self side, i.e., to actually fire, must also explicitly check canRequest
    boolean canAccept(Role self, EAcc<?> a);

    boolean canDisconnect(Role self, EDisconnect<?> d);

    // N.B. "sync" action but only considers the self side, i.e., to actually fire, must also explicitly check canServerWrap
    // N.B. doesn't actually change any state
    boolean canClientWrap(Role self, EClientWrap<?> cw);

    // N.B. "sync" action but only considers the self side, i.e., to actually fire, must also explicitly check canClientWrap
    // N.B. doesn't actually change queues state
    boolean canServerWrap(Role self, EServerWrap<?> sw);

    // Pre: canSend, e.g., via via SConfig.getFireable
    // Return an updated copy
    SBuffers send(Role self, ESend<?> a);

    // Pre: canReceive, e.g., via SConfig.getFireable
    // Return an updated copy
    SBuffers receive(Role self, ERecv<?> a);

    // Sync action
    // Pre: canRequest(r1, [[r2]]) and canAccept(r2, [[r1]]), where [[r]] is a matching action with peer r -- e.g., via via SConfig.getFireable
    // Return an updated copy
    SBuffers connect(Role r1, Role r2);  // Role sides and message don't matter

    // Pre: canDisconnect(self, d), e.g., via SConfig.via getFireable
    // Return an updated copy
    SBuffers disconnect(Role self, EDisconnect<?> d);

    // N.B. direction sensitive (viz., after some disconnect)
    boolean isConnected(Role self, Role peer);

    boolean isEmpty(Role r);  // this.connected doesn't matter

    // Return a (deep) copy -- currently, checkEventualReception expects a modifiable return
    Map<Role, Map<Role, List<ESend<DynamicActionKind>>>> getQueues();

    Map<Role, List<ESend<DynamicActionKind>>> getQueue(Role r);
}
