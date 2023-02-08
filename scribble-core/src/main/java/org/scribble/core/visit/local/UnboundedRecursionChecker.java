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
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.session.*;
import org.scribble.core.type.session.local.LSend;
import org.scribble.core.type.session.local.LSeq;
import org.scribble.core.type.session.local.LType;
import org.scribble.core.visit.STypeVisitor;
import org.scribble.util.ScribException;

import java.util.*;

// Cf. ReachabilityChecker
public class UnboundedRecursionChecker extends STypeVisitor<Local, LSeq> {

    private UnboundedRecursionEnv env;

    protected UnboundedRecursionChecker() {
        this.env = new UnboundedRecursionEnv();
    }

    @Override
    public SVisitable<Local, LSeq> visitDirectedInteraction(DirectedInteraction<Local, LSeq> n)
            throws ScribException {
        UnboundedRecursionEnv env = getEnv();
        if (n instanceof LSend) {
            Set<RecVar> out = new HashSet<>(env.out);
            env.recvars.forEach(x -> out.add(x));
            setEnv(new UnboundedRecursionEnv(env.recvars, env.in, out));
        } else {
            Set<RecVar> in = new HashSet<>(env.in);
            env.recvars.forEach(x -> in.add(x));
            setEnv(new UnboundedRecursionEnv(env.recvars, in, env.out));
        }
        return n;
    }

    /*@Override
    public SVisitable<Local, LSeq> visitDisconnect(DisconnectAction<Local, LSeq> n) throws ScribException {
        return n;
    }*/

    @Override
    public SVisitable<Local, LSeq> visitChoice(Choice<Local, LSeq> n)
            throws ScribException {
        UnboundedRecursionEnv entry = getEnv();
        List<UnboundedRecursionEnv> blocks = new LinkedList<>();
        UnboundedRecursionChecker nested = new UnboundedRecursionChecker();
        for (LSeq block : n.getBlocks()) {
            nested.setEnv(entry);
            block.accept(nested);
            blocks.add(nested.getEnv());
        }
        UnboundedRecursionEnv first = blocks.get(0);
        Set<RecVar> in = new HashSet<>(first.in);
        Set<RecVar> out = new HashSet<>(first.out);
        for (UnboundedRecursionEnv e : blocks.subList(1, blocks.size())) {
            in.retainAll(e.in);
            out.addAll(e.out);
        }
        setEnv(new UnboundedRecursionEnv(entry.recvars, in, out));
        return n;
    }

    public SVisitable<Local, LSeq> visitContinue(Continue<Local, LSeq> n) throws ScribException {
        UnboundedRecursionEnv env = getEnv();
        RecVar rv = n.getRecVar();
        if ((env.out.contains(rv) && !env.in.contains(rv))
                || (!env.out.contains(rv) && env.in.contains(rv))) {
            throw new UnboundedRecursionException("Potentially unbounded output recursion: " + n);
        }

        return n;
    }

    @Override
    public final SVisitable<Local, LSeq> visitDo(Do<Local, LSeq> n) throws ScribException {
        throw new RuntimeException(this.getClass() + " unsupported for Do: " + n);
    }

    @Override
    public SVisitable<Local, LSeq> visitRecursion(Recursion<Local, LSeq> n)
            throws ScribException {
        RecVar rv = n.getRecVar();
        UnboundedRecursionEnv orig = getEnv();
        Set<RecVar> recvars = new HashSet<>(orig.recvars);
        recvars.add(rv);
        UnboundedRecursionEnv entry = new UnboundedRecursionEnv(recvars, orig.in, orig.out);
        UnboundedRecursionChecker nested = new UnboundedRecursionChecker();
        nested.setEnv(entry);
        try {
            n.getBody().accept(nested);
        } catch (UnboundedRecursionException e) {
            throw new UnboundedRecursionException("Potentially unbounded output recursion:\n" + n);
        }
        UnboundedRecursionEnv visited = nested.getEnv();
        Set<RecVar> in = new HashSet<>(visited.in);
        Set<RecVar> out = new HashSet<>(visited.out);
        in.remove(rv);
        out.remove(rv);
        setEnv(new UnboundedRecursionEnv(orig.recvars, in, out));
        return n;
    }

    @Override
    public LSeq visitSeq(LSeq n) throws ScribException {
        for (LType t : n.getElements()) {
            t.accept(this);
        }
        return n;
    }

    protected UnboundedRecursionEnv getEnv() {
        return this.env;
    }

    protected void setEnv(UnboundedRecursionEnv env) {
        this.env = env;
    }
}


// Immutable
class UnboundedRecursionEnv {

    public final Set<RecVar> recvars;
    public final Set<RecVar> in;
    public final Set<RecVar> out;

    public UnboundedRecursionEnv() {
        this.recvars = Collections.emptySet();
        this.in = Collections.emptySet();
        this.out = Collections.emptySet();
    }

    public UnboundedRecursionEnv(Set<RecVar> recvars, Set<RecVar> in, Set<RecVar> out) {
        this.recvars = Collections.unmodifiableSet(new HashSet<>(recvars));
        this.in = Collections.unmodifiableSet(new HashSet<>(in));
        this.out = Collections.unmodifiableSet(new HashSet<>(out));
    }
}