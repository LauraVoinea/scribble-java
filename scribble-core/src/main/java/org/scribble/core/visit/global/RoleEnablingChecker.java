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
package org.scribble.core.visit.global;

import org.scribble.core.type.kind.Global;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.*;
import org.scribble.core.type.session.global.GSeq;
import org.scribble.core.visit.STypeVisitor;
import org.scribble.util.ScribException;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

// Pre: use on inlined or later (unsupported for Do, also Protocol)
public class RoleEnablingChecker extends STypeVisitor<Global, GSeq> {
    private Set<Role> enabled;  // Invariant: unmodifiable

    protected RoleEnablingChecker(Set<Role> enabled) {
        setEnabled(enabled);
    }

    public SVisitable<Global, GSeq> visitChoice(Choice<Global, GSeq> n)
            throws ScribException {
        Role subj0 = n.getSubject();
        Set<Role> enabled = getEnabled();
        if (!enabled.contains(subj0)) {
            throw new ScribException("Subject not enabled: " + subj0);
        }
        Set<Role> subj = Stream.of(subj0).collect(Collectors.toSet());
        RoleEnablingChecker nested = new RoleEnablingChecker(subj);
        // Arg redundant, but better to keep a single constructor, for factory pattern
        List<Set<Role>> blocks = new LinkedList<>();
        for (GSeq block : n.getBlocks()) {
            nested.setEnabled(subj);  // Copies defensively

            //nested.visitSeq(block);
            block.accept(nested);

            blocks.add(nested.getEnabled());
        }
        Set<Role> res = new HashSet<>(enabled);
        Set<Role> tmp = blocks.stream().flatMap(x -> x.stream())
                .filter(x -> blocks.stream().allMatch(y -> y.contains(x)))
                .collect(Collectors.toSet());
        res.addAll(tmp);
        setEnabled(res);
        return n;
    }

    @Override
    public SVisitable<Global, GSeq> visitDirectedInteraction(
            DirectedInteraction<Global, GSeq> n) throws ScribException {
        Set<Role> enabled = getEnabled();
        if (!enabled.contains(n.src)) {
            throw new ScribException("Source role not enabled: " + n.src);
        }
        if (enabled.contains(n.dst)) {
            return n;
        }
        Set<Role> res = new HashSet<>(enabled);
        res.add(n.dst);
        setEnabled(res);
        return n;
    }

    @Override
    public final SVisitable<Global, GSeq> visitDo(Do<Global, GSeq> n) throws ScribException {
        throw new RuntimeException(this.getClass() + " unsupported for Do: " + n);
    }

    @Override
    public SVisitable<Global, GSeq> visitDisconnect(
            DisconnectAction<Global, GSeq> n) throws ScribException {
        Set<Role> enabled = getEnabled();
        if (!enabled.contains(n.left)) {
            throw new ScribException("Role not enabled: " + n.left);
        }
        if (!enabled.contains(n.right)) {
            throw new ScribException("Role not enabled: " + n.right);
        }
        return n;
    }

    public Set<Role> getEnabled() {
        return this.enabled;
    }

    // Guards this.enabled unmodifiable
    protected void setEnabled(Set<Role> enabled) {
        this.enabled = Collections.unmodifiableSet(enabled);
    }
}
