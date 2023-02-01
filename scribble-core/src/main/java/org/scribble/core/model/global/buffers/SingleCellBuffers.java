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

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.endpoint.actions.*;
import org.scribble.core.type.name.Role;

import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import java.util.stream.Stream;

// Immutable -- send/receive/etc return updated copies
public class SingleCellBuffers implements SBuffers {

    private final Map<Role, Map<Role, Boolean>> connected = new HashMap<>();
    // local -> peer -> does-local-consider-connected  (symmetric)
    // CHECKME: refactor as Map<Role, Set<Role>> ?  cf. ConnectionChecker

    private final Map<Role, Map<Role, ESend<DynamicActionKind>>> buffs = new HashMap<>();
    // dest -> src -> msg -- N.B. connected.get(A).get(B) => can send into buffs.get(B).get(A) ("reversed")
    // N.B. hardcoded to capacity one -- SQueues would be the generalisation
    // null ESend for empty queue

    public SingleCellBuffers(Set<Role> roles, boolean implicit) {
        for (Role r1 : roles) {
            HashMap<Role, Boolean> connected = new HashMap<>();
            HashMap<Role, ESend<DynamicActionKind>> queues = new HashMap<>();
            for (Role r2 : roles) {
                if (!r1.equals(r2)) {
                    connected.put(r2, implicit);
                    queues.put(r2, null);  // null for empty queue
                }
            }
            this.connected.put(r1, connected);
            this.buffs.put(r1, queues);
        }
    }

    protected SingleCellBuffers(SingleCellBuffers queues) {
        for (Role r : queues.buffs.keySet()) {
            this.connected.put(r, new HashMap<>(queues.connected.get(r)));
            this.buffs.put(r, new HashMap<>(queues.buffs.get(r)));
        }
    }

    @Override
    public boolean canSend(Role self, ESend<?> a) {
        return isConnected(self, a.peer)
                //&& isConnected(a.peer, self)  // CHECKME: only consider local side?
                && this.buffs.get(a.peer).get(self) == null;
    }

    @Override
    public boolean canReceive(Role self, ERecv<?> a) {
        ESend<DynamicActionKind> send = this.buffs.get(self).get(a.peer);
        return isConnected(self, a.peer)
                // Other direction doesn't matter, local can still receive after peer disconnected
                && send != null && a.toDynamicDual(send.peer).equals(send);
    }

    // N.B. "sync" action but only considers the self side, i.e., to actually fire, must also explicitly check canAccept
    @Override
    public boolean canRequest(Role self, EReq<?> c) {
        return !isConnected(self, c.peer);
    }

    // N.B. "sync" action but only considers the self side, i.e., to actually fire, must also explicitly check canRequest
    @Override
    public boolean canAccept(Role self, EAcc<?> a) {
        return !isConnected(self, a.peer);
    }

    public boolean canDisconnect(Role self, EDisconnect<?> d) {
        return isConnected(self, d.peer);
    }

    // N.B. "sync" action but only considers the self side, i.e., to actually fire, must also explicitly check canServerWrap
    // N.B. doesn't actually change any state
    @Override
    public boolean canClientWrap(Role self, EClientWrap<?> cw) {
        return isConnected(self, cw.peer);
    }

    // N.B. "sync" action but only considers the self side, i.e., to actually fire, must also explicitly check canClientWrap
    // N.B. doesn't actually change queues state
    public boolean canServerWrap(Role self, EServerWrap<?> sw) {
        return isConnected(self, sw.peer);
    }

    // Pre: canSend, e.g., via via SConfig.getFireable
    // Return an updated copy
    @Override
    public SingleCellBuffers send(Role self, ESend<?> a) {
        SingleCellBuffers copy = new SingleCellBuffers(this);
        copy.buffs.get(a.peer).put(self, a.toDynamic());
        return copy;
    }

    // Pre: canReceive, e.g., via SConfig.getFireable -- doesn't check message op
    // Return an updated copy
    @Override
    public SingleCellBuffers receive(Role self, ERecv<?> a) {
        SingleCellBuffers copy = new SingleCellBuffers(this);
        copy.buffs.get(self).put(a.peer, null);
        return copy;
    }

    // Sync action
    // Pre: canRequest(r1, [[r2]]) and canAccept(r2, [[r1]]), where [[r]] is a matching action with peer r -- e.g., via via SConfig.getFireable
    // Return an updated copy
    @Override
    public SingleCellBuffers connect(Role r1, Role r2)  // Role sides and message don't matter
    {
        SingleCellBuffers copy = new SingleCellBuffers(this);
        copy.connected.get(r1).put(r2, true);
        copy.connected.get(r2).put(r1, true);
        return copy;
    }

    // Pre: canDisconnect(self, d), e.g., via SConfig.via getFireable
    // Return an updated copy
    @Override
    public SingleCellBuffers disconnect(Role self, EDisconnect<?> d) {
        SingleCellBuffers copy = new SingleCellBuffers(this);
        copy.connected.get(self).put(d.peer, false);  // Didn't update buffs (cf. SConfig.getOrphanMessages)
        return copy;
    }

    // N.B. direction sensitive (viz., after some disconnect)
    @Override
    public boolean isConnected(Role self, Role peer) {
        return this.connected.get(self).get(peer);
    }

    @Override
    public boolean isEmpty(Role r)  // this.connected doesn't matter
    {
        return this.buffs.get(r).values().stream().allMatch(Objects::isNull);
    }

    // Return a (deep) copy -- currently, checkEventualReception expects a modifiable return
    // N.B. hardcoded to capacity one
    @Override
    public Map<Role, Map<Role, List<ESend<DynamicActionKind>>>> getQueues() {
        /*return this.buffs.entrySet().stream().collect(Collectors.toMap(
                Entry::getKey,
                x -> new HashMap<>(x.getValue())));  // Collections.unmodifiableMap(x.getValue())*/
        Map<Role, Map<Role, List<ESend<DynamicActionKind>>>> collect =
                this.buffs.entrySet().stream().collect(Collectors.toMap(
                        Entry::getKey,
                        x -> x.getValue().entrySet().stream().collect(Collectors.toMap(
                                Entry::getKey,
                                y -> {
                                    ESend<DynamicActionKind> v = y.getValue();
                                    return v == null
                                            ? Collections.emptyList()
                                            : Stream.of(v).collect(Collectors.toList());
                                }))));
        return collect;
    }

    // N.B. hardcoded to capacity one
    @Override
    public Map<Role, List<ESend<DynamicActionKind>>> getQueue(Role r) {
        return this.buffs.get(r).entrySet().stream().collect(Collectors.toMap(
                Entry::getKey,
                x -> {
                    ESend<DynamicActionKind> v = x.getValue();
                    return v == null
                            ? Collections.emptyList()
                            : Stream.of(v).collect(Collectors.toList());
                }
        ));
    }

    @Override
    public final int hashCode() {
        int hash = 131;
        hash = 31 * hash + this.connected.hashCode();
        hash = 31 * hash + this.buffs.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof SingleCellBuffers)) {
            return false;
        }
        SingleCellBuffers b = (SingleCellBuffers) o;
        return this.connected.equals(b.connected) && this.buffs.equals(b.buffs);
    }

    @Override
    public String toString() {
        return this.buffs.entrySet().stream()
                .filter(e -> e.getValue().values().stream().anyMatch(Objects::nonNull))
                .collect(Collectors.toMap(
                        Entry::getKey,
                        e -> e.getValue().entrySet().stream()
                                .filter(f -> f.getValue() != null)
                                .collect(Collectors.toMap(Entry::getKey, Entry::getValue))
                )).toString();
    }
}
