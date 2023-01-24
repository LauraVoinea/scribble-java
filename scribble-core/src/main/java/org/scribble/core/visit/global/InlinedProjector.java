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

import org.scribble.core.job.Core;
import org.scribble.core.type.kind.Global;
import org.scribble.core.type.kind.Local;
import org.scribble.core.type.name.*;
import org.scribble.core.type.session.*;
import org.scribble.core.type.session.global.GConnect;
import org.scribble.core.type.session.global.GSeq;
import org.scribble.core.type.session.local.*;
import org.scribble.core.visit.STypeAggNoThrow;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

// Pre: use on inlined (i.e., "do" inlined, roles pruned)
public class InlinedProjector extends STypeAggNoThrow<Global, GSeq, LType> {

    //protected final Set<RecVar> unguarded;
    protected final Set<RecVar> unguarded;
    // Projection "prunes" unguarded continues from choice cases, e.g., mu X.(A->B:1.X + A->B:2.A->C:2) for C, i.e., travel agent
    // ...this can be followed by RecPruner to remove recs that are "orphaned" by continue pruning
    // N.B. projection does not "merge" choice cases in , e.g., rec X { 1() from A; choice at A { continue X; } or { continue X; } }...
    // ...that is a non-det branch, same as rec X { choice at A { 1() from A; } or { 1() from A; } }

    public final Core core;
    public final Role self;

    protected InlinedProjector(Core core, Role self) {
        this.core = core;
        this.self = self;
        this.unguarded = new HashSet<>();
    }

    // Copy constructor for dup
    protected InlinedProjector(InlinedProjector v) {
        this.core = v.core;
        this.self = v.self;
        this.unguarded = new HashSet<>(v.unguarded);
    }

    // Use to visit a node with a copy of the current context (this.unguarded), e.g., for "nested" context visiting (Choice)
    // N.B. subclasses should override
    protected InlinedProjector dup() {
        return new InlinedProjector(this);
    }

    @Override
    public LType visitChoice(Choice<Global, GSeq> n) {
        List<LSeq> blocks = n.getBlocks().stream()
                .map(x -> dup().visitSeq(x))
                .filter(x -> !x.isEmpty() && !isUnguardedSingleContinue(x))
                .collect(Collectors.toList());
        if (blocks.isEmpty()) {
            return LSkip.SKIP; // CHECKME: OK, or "empty" choice at subj still important?
        }
        this.unguarded.clear();  // At least one block is non-empty, consider continues guarded -- must clear here, blocks visited using dup's

        //InlinedEnablerInferer v = new InlinedEnablerInferer(this.unguarded);
        Role subj = n.getSubject();
        if (subj.equals(this.self)) {
            subj = Role.SELF;  // i.e., internal choice
        }  // CHECKME: consistent ext choice means can infer from any one seq?
        // CHECKME: "self" also explcitily used for Do, but implicitly for MessageTransfer, inconsistent?
        return this.core.config.tf.local.LChoice(null, subj, blocks);
    }

    // N.B. won't prune unguarded continues that have a bad sequence, will be caught later by reachability checking (e.g., bad.reach.globals.gdo.Test04)
    private boolean isUnguardedSingleContinue(LSeq block) {
        List<LType> elems = block.getElements();
        if (elems.size() != 1) {
            return false;
        }
        SVisitable<Local, LSeq> e = elems.get(0);
        return (e instanceof LContinue)
                && this.unguarded.contains(((LContinue) e).getRecVar());  // Bound recvars already checked
    }

    @Override
    public LType visitContinue(Continue<Global, GSeq> n) {
        return this.core.config.tf.local.LContinue(null, n.getRecVar());
    }

    @Override
    public LType visitDirectedInteraction(DirectedInteraction<Global, GSeq> n) {
		/*if (n.src.equals(self) && n.dst.equals(self))
		{
				// CHECKME: already checked?
		}*/
        LTypeFactory lf = this.core.config.tf.local;
        if (n instanceof GConnect)  // FIXME
        {
            return n.src.equals(self) ? lf.LReq(null, n.msg, n.dst)
                    : n.dst.equals(self) ? lf.LAcc(null, n.src, n.msg)
                    : LSkip.SKIP;
        } else //if (n instanceof GMessageTransfer)
        {
            return n.src.equals(self) ? lf.LSend(null, n.msg, n.dst)
                    : n.dst.equals(self) ? lf.LRecv(null, n.src, n.msg)
                    : LSkip.SKIP;
        }
    }

    @Override
    public LType visitDisconnect(DisconnectAction<Global, GSeq> n) {
		/*if (n.src.equals(self) && n.dst.equals(self))
		{
				// CHECKME: already checked?
		}*/
        LTypeFactory lf = this.core.config.tf.local;
        return n.left.equals(self) ? lf.LDisconnect(null, n.right)
                : n.right.equals(self) ? lf.LDisconnect(null, n.left)
                : LSkip.SKIP;
    }

    @Override
    public LType visitDo(Do<Global, GSeq> n) {
        throw new RuntimeException("Unsupported for Do: " + n + " ,, " + this.getClass());
    }

    @Override
    public LType visitRecursion(Recursion<Global, GSeq> n) {
        RecVar recvar0 = n.getRecVar();
        GSeq body0 = n.getBody();
        this.unguarded.add(recvar0);
        LSeq body = visitSeq(body0);  // Single "nested" Seq, don't need to copy visitor
        if (body.isEmpty())  // A simple special case of empty-rec pruning -- leave it to "official" rec pruning?
        {
            return LSkip.SKIP;
        }
        Set<RecVar> rvs = new HashSet<>();
        rvs.add(recvar0);
        //if (body.visitWithNoThrow(new SingleContinueChecker(rvs)))
        // "Generalised" single-continue checked now unnecessary, single-continues pruned in choice visiting above
        List<LType> elems = body.getElements();
        if (elems.size() == 1) {
            SVisitable<Local, LSeq> e = elems.get(0);
            if (e instanceof LContinue
                    && ((LContinue) e).getRecVar().equals(recvar0)) {
                return LSkip.SKIP;
            }
        }
        this.unguarded.remove(recvar0);
        return this.core.config.tf.local.LRecursion(null, recvar0, body);
    }

    // Param "hardcoded" to B (cf. Seq, or SType return) -- this visitor pattern depends on B for Choice/Recursion/etc reconstruction
    @Override
    public LSeq visitSeq(GSeq n) {
        List<LType> elems = new LinkedList<>();
        for (SVisitable<Global, GSeq> e : n.getElements()) {
            LType e1 = e.visitWithNoThrow(this);
            if (!(e1 instanceof LSkip)) {
                elems.add(e1);
                this.unguarded.clear();
            }
        }
        return this.core.config.tf.local.LSeq(null, elems);
        // Empty seqs converted to LSkip by GChoice/Recursion projection
        // And a WF top-level protocol cannot produce empty LSeq
        // So a projection never contains an empty LSeq -- i.e., "empty choice/rec" pruning unnecessary
    }

    // CHECKME: relocate?
    public static LProtoName getSimpledProjectionName(ProtoName<Global> simpname,
                                                      Role role) {
        return new LProtoName(simpname.toString() + "_" + role.toString());
    }

    // Role is the target subprotocol parameter (not the current projector self -- actually the self just popped) -- ?
    public static LProtoName getFullProjectionName(ProtoName<Global> fullname,
                                                   Role role) {
        ProtoName<Local> simplename = InlinedProjector.getSimpledProjectionName(
                fullname.getSimpleName(), role);
        ModuleName modname = getProjectionModuleName(fullname.getPrefix(),
                simplename);
        return new LProtoName(modname, simplename);
    }

    // fullname is the un-projected name; localname is the already projected simple name
    public static ModuleName getProjectionModuleName(ModuleName fullname,
                                                     ProtoName<Local> localname) {
        ModuleName simpname = new ModuleName(
                fullname.getSimpleName().toString() + "_" + localname.toString());
        return new ModuleName(fullname.getPrefix(), simpname); // Supports unary fullname
    }

    @Override
    protected LType unit(SVisitable<Global, GSeq> n) {
        throw new RuntimeException("Unsupported for InlinedProjector: " + n);
    }

    @Override
    protected LType agg(SVisitable<Global, GSeq> n, Stream<LType> ts) {
        throw new RuntimeException(
                "Unsupported for InlinedProjector: " + n + " ,, " + ts);
    }
}
