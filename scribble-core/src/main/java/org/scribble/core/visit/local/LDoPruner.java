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

package org.scribble.core.visit.local;

import org.scribble.core.job.Core;
import org.scribble.core.lang.SubprotoSig;
import org.scribble.core.lang.local.LProtocol;
import org.scribble.core.type.kind.Local;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Choice;
import org.scribble.core.type.session.Do;
import org.scribble.core.type.session.Recursion;
import org.scribble.core.type.session.SVisitable;
import org.scribble.core.type.session.local.LSeq;
import org.scribble.core.type.session.local.LSkip;
import org.scribble.core.type.session.local.LType;
import org.scribble.core.visit.STypeVisitorNoThrow;

import java.util.*;
import java.util.stream.Collectors;



// Pre: LRoleDeclAndDoArgFixer (for LSubprotoVisitorNoThrow visiting pattern)
// Cf. RecPruner
public class LDoPruner //extends DoPruner<Local, LSeq>
        extends STypeVisitorNoThrow<Local, LSeq>
        implements LSubprotoVisitorNoThrow<SVisitable<Local, LSeq>> {

    protected final Core core;

    private Role self;

    protected final Deque<SubprotoSig> stack;

    protected final Set<SubprotoSig> unguarded;  // Cf. InlinedProjector (unguarded-continue pruning)

    protected LDoPruner(Core core) {
        this.core = core;
        this.stack = new LinkedList<>();
        this.unguarded = new HashSet<>();
    }

    protected LDoPruner(LDoPruner v) {
        this.core = v.core;
        this.self = v.self;
        this.stack = new LinkedList<>(v.stack);
        this.unguarded = new HashSet<>(v.unguarded);
    }

    // Top-level entry, different to the other visit methods
    public LProtocol visitLProtocol(LProtocol n) {
        this.self = n.self;
        this.stack.clear();
        this.unguarded.clear();

        SubprotoSig sig = new SubprotoSig(n);  // N.B. overloaded constructor, param type important
        this.stack.push(sig);
        this.unguarded.add(sig);

        //LSeq def = visitSeq(n.def);
        LSeq def = (LSeq) n.def.acceptNoThrow(this);

        return n.reconstruct(n.getSource(), n.mods, n.fullname, n.roles,
                n.self, n.params, def);
    }

    @Override
    public SVisitable<Local, LSeq> visitChoice(Choice<Local, LSeq> n) {
        // Duplicated from InlinedProjector.visitChoice
        List<LSeq> blocks = n.getBlocks().stream()

                //.map(x -> new LDoPruner(this).visitSeq(x)).filter(x -> !x.isEmpty())
                .map(x -> (LSeq) x.acceptNoThrow(new LDoPruner(this))).filter(x -> !x.isEmpty())

                .collect(Collectors.toList());
        if (blocks.isEmpty()) {
            return LSkip.SKIP;  // N.B. returning a Seq -- handled by visitSeq (similar to LSkip for locals)
        }
        this.unguarded.remove(this.stack.peek());
        return n.reconstruct(n.getSource(), n.getSubject(), blocks);
    }

    @Override
    public SVisitable<Local, LSeq> visitDo(Do<Local, LSeq> n) {
        SubprotoSig sig = new SubprotoSig(n.getProto(), n.getRoles(), n.getArgs());
        if (this.stack.contains(sig)) {
            return this.unguarded.contains(sig) ? LSkip.SKIP : n;
        }

        // Duplicated from SubprotoRoleCollector
        this.stack.push(sig);

        //LSeq def = visitSeq(prepareSubprotoForVisit(this.core, n, true));
        LSeq def = (LSeq) (prepareSubprotoForVisit(this.core, n, true)).acceptNoThrow(this);

        // true (passive) to ignore non-fixed ext-choice subjs (e.g., good.efsm.gdo.Test11)
        // Changes ultimately discarded: "nested" entries only do "info collection", actual AST modifications only recoded for the top-level Projection (cf. visitProjection)
        this.stack.pop();
        return def.isEmpty() ? LSkip.SKIP : n;  // Cf. unit(n)
    }

    @Override
    public SVisitable<Local, LSeq> visitRecursion(Recursion<Local, LSeq> n) {

        //LSeq body = visitSeq(n.getBody());
        LSeq body = (LSeq) n.getBody().acceptNoThrow(this);

        return body.isEmpty()
                ? LSkip.SKIP
                : n.reconstruct(n.getSource(), n.getRecVar(), body);
    }

    @Override
    public LSeq visitSeq(LSeq n) {
        // Duplicated from InlinedProjector.visitSeq
        List<LType> elems = new LinkedList<>();
        for (LType e : n.getElements()) {
            LType e1 = (LType) e.acceptNoThrow(this);
            if (!(e1 instanceof LSkip)) {
                elems.add(e1);
                this.unguarded.clear();
            }
        }
        return this.core.config.tf.local.LSeq(null, elems);
    }
}