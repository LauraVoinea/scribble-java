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

import org.scribble.core.type.kind.Local;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.*;
import org.scribble.core.type.session.local.LDisconnect;
import org.scribble.core.type.session.local.LSend;
import org.scribble.core.type.session.local.LSeq;
import org.scribble.core.type.session.local.LType;
import org.scribble.core.visit.STypeVisitor;
import org.scribble.util.ScribException;

import java.util.*;

// TODO incomplete -- need to do input sequence counting also
// Cf. ReachabilityChecker
@Deprecated
public class OutputSequenceCounter extends STypeVisitor<Local, LSeq> {

    private OutputSequenceEnv env;

    protected OutputSequenceCounter() {
        this.env = new OutputSequenceEnv();
    }

    @Override
    public SVisitable<Local, LSeq> visitDirectedInteraction(DirectedInteraction<Local, LSeq> n)
            throws ScribException {
        OutputSequenceEnv env = getEnv();
        Map<Role, Boolean> polarity = new HashMap<>(env.polarity);
        Map<Role, Integer> counts = new HashMap<>(env.counts);
        Map<Role, Integer> max = new HashMap<>(env.max);
        if (n instanceof LSend) {
            polarity.put(n.dst, true);
            Integer c = counts.get(n.dst);
            if (c == null) {
                c = 0;
            }
            counts.put(n.dst, c + 1);
            Integer m = max.get(n.dst);
            if (m == null || c + 1 > m) {
                max.put(n.dst, c + 1);
            }
        } else {
            polarity.put(n.src, false);
            counts.put(n.src, 0);
            if (!max.containsKey(n.src)) {
                max.put(n.src, 0);
            }
        }
        setEnv(new OutputSequenceEnv(polarity, counts, max));
        return n;
    }

    @Override
    public SVisitable<Local, LSeq> visitDisconnect(DisconnectAction<Local, LSeq> n)
            throws ScribException {
        LDisconnect cast = (LDisconnect) n;
        Role r = cast.getPeer();
        Map<Role, Boolean> polarity = new HashMap<>(env.polarity);
        Map<Role, Integer> counts = new HashMap<>(env.counts);
        Map<Role, Integer> max = new HashMap<>(env.max);
        polarity.put(r, false);
        counts.put(r, 0);
        if (!max.containsKey(r)) {
            max.put(r, 0);
        }
        setEnv(new OutputSequenceEnv(polarity, counts, max));
        return n;
    }

    @Override
    public SVisitable<Local, LSeq> visitChoice(Choice<Local, LSeq> n)
            throws ScribException {
        OutputSequenceEnv entry = getEnv();

        List<OutputSequenceEnv> blocks = new LinkedList<>();
        OutputSequenceCounter nested = new OutputSequenceCounter();
        for (LSeq block : n.getBlocks()) {
            nested.setEnv(entry);
            block.visitWith(nested);
            blocks.add(nested.getEnv());
        }

        Map<Role, Boolean> p = new HashMap<>();
        Map<Role, Integer> c = new HashMap<>();
        Map<Role, Integer> m = new HashMap<>();
        // All roles of entry are also present here
        blocks.stream().flatMap(x -> x.counts.keySet().stream()).forEach(r -> {
            // x below can be null because, e.g., local choice at B with no B interaction (e.g., good.safety.stuckmsg.fourparty)
            boolean polarity = blocks.stream()
                    .anyMatch(x -> x.polarity.containsKey(r) && x.polarity.get(r));
            p.put(r, polarity);

            int count = blocks.stream().map(x -> x.counts.get(r))
                    .filter(x -> x != null)
                    .max(Integer::compare).get();
            c.put(r, count);

            int max = blocks.stream().map(x -> x.max.get(r))
                    .filter(x -> x != null)
                    .max(Integer::compare).get();
            m.put(r, max);
        });

        setEnv(new OutputSequenceEnv(p, c, m));
        return n;
    }

    /*public SVisitable<Local, LSeq> visitContinue(Continue<Local, LSeq> n)
            throws ScribException {
        return n;
    }*/

    @Override
    public final SVisitable<Local, LSeq> visitDo(Do<Local, LSeq> n) throws ScribException {
        throw new RuntimeException(this.getClass() + " unsupported for Do: " + n);
    }

    /*@Override
    public SVisitable<Local, LSeq> visitRecursion(Recursion<Local, LSeq> n)
            throws ScribException {
        n.getBody().visitWith(this);
        return n;
    }*/

    @Override
    public LSeq visitSeq(LSeq n) throws ScribException {
        for (LType t : n.getElements()) {
            t.visitWith(this);
        }
        return n;
    }

    public Map<Role, Integer> getMaxCounts() {
        return this.env.max;
    }

    protected OutputSequenceEnv getEnv() {
        return this.env;
    }

    protected void setEnv(OutputSequenceEnv env) {
        this.env = env;
    }
}


// Immutable
class OutputSequenceEnv {

    public final Map<Role, Boolean> polarity;  // true=output, false=not output
    public final Map<Role, Integer> counts;
    public final Map<Role, Integer> max;

    public OutputSequenceEnv() {
        this.polarity = Collections.emptyMap();
        this.counts = Collections.emptyMap();
        this.max = Collections.emptyMap();
    }

    public OutputSequenceEnv(
            Map<Role, Boolean> polarity, Map<Role, Integer> counts, Map<Role, Integer> max) {
        this.polarity = Collections.unmodifiableMap(new HashMap<>(polarity));
        this.counts = Collections.unmodifiableMap(new HashMap<>(counts));
        this.max = Collections.unmodifiableMap(new HashMap<>(max));
    }
}