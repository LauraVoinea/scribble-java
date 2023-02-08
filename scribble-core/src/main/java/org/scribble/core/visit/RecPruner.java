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

package org.scribble.core.visit;

import org.scribble.core.type.kind.ProtoKind;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.session.Choice;
import org.scribble.core.type.session.Recursion;
import org.scribble.core.type.session.SVisitable;
import org.scribble.core.type.session.Seq;
import org.scribble.core.visit.gather.RecVarGatherer;

import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

// Assumes no shadowing, e.g., use after inlining recvar disamb (also used after projection)
public class RecPruner<K extends ProtoKind, B extends Seq<K, B>>
        extends STypeVisitorNoThrow<K, B> {

    protected RecPruner() {
    }

    @Override
    public SVisitable<K, B> visitChoice(Choice<K, B> n) {
        List<B> blocks0 = n.getBlocks();
        List<B> blocks = blocks0.stream().map(x -> visitSeq(x))
                .filter(x -> !x.isEmpty()).collect(Collectors.toList());
        if (blocks.isEmpty()) {
            return blocks0.get(0).reconstruct(null, blocks);  // N.B. returning a Seq -- handled by visitSeq (similar to LSkip for locals)
        }
        return n.reconstruct(n.getSource(), n.getSubject(), blocks);
    }

    @Override
    public SVisitable<K, B> visitRecursion(Recursion<K, B> n) {
        // Assumes no shadowing (e.g., use after SType#getInlined recvar disamb)
        RecVar recvar = n.getRecVar();
        B body = n.getBody();

        //Set<RecVar> rvs = body.gather(new RecVarGatherer<K, B>()::visit)
        Set<RecVar> rvs = body.acceptNoThrow(new RecVarGatherer<>())

                .collect(Collectors.toSet());
        return rvs.contains(recvar)
                ? n.reconstruct(n.getSource(), recvar, visitSeq(body))
                : body;  // i.e., return a Seq, to be "inlined" by Seq.pruneRecs -- N.B. must handle empty Seq case
    }

    @Override
    public B visitSeq(B n) {
        List<SVisitable<K, B>> elems = new LinkedList<>();
        for (SVisitable<K, B> e : n.getElements()) {
            SVisitable<K, B> e1 = (SVisitable<K, B>) e.acceptNoThrow(this);
            if (e1 instanceof Seq<?, ?>) {  // cf. visitRecursion  (also cf. LSkip)
                elems.addAll(((Seq<K, B>) e1).getElements());  // Handles empty Seq case
            } else {
                elems.add(e1);
            }
        }
        return n.reconstruct(n.getSource(), elems);
    }
}
