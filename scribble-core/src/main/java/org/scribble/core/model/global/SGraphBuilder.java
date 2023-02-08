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

import org.scribble.core.job.Core;
import org.scribble.core.job.CoreArgs;
import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.StaticActionKind;
import org.scribble.core.model.endpoint.EFsm;
import org.scribble.core.model.endpoint.EGraph;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.model.global.buffers.SBuffers;
import org.scribble.core.model.global.buffers.SQueues;
import org.scribble.core.model.global.buffers.SingleCellBuffers;
import org.scribble.core.type.kind.Global;
import org.scribble.core.type.name.ProtoName;
import org.scribble.core.type.name.Role;
import org.scribble.util.ScribException;

import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Collectors;

public class SGraphBuilder {
    public final Core core;

    private final SGraphBuilderUtil util;

    public SGraphBuilder(Core core) {
        this.core = core;
        this.util = this.core.config.mf.global.SGraphBuilderUtil();
    }

    // Do as an initial state rather than config?
    protected SConfig createInitConfig(
            Map<Role, EGraph> egraphs, boolean explicit, ProtoName<Global> fullname) {
        Map<Role, EFsm> efsms = egraphs.entrySet().stream()
                .collect(Collectors.toMap(Entry::getKey, e -> e.getValue().toFsm()));
        SBuffers b0;
        if (this.core.config.hasFlag(CoreArgs.SCRIBBLE_UNBOUNDED_BUFFERS)) {  // TODO refactor
            /*if (this.core.getContext().isPotentiallyUnbounded(fullname)) {
                /*throw new RuntimeScribException(
                        "Potentially unbounded protocol, aborting model construction:\n"
                                + fullname);* /
            }*/
            b0 = new SQueues(efsms.keySet(), !explicit);
        } else {
            b0 = new SingleCellBuffers(efsms.keySet(), !explicit);
        }
        return this.core.config.mf.global.SConfig(efsms, b0);
    }

    // Factory method: not fully integrated with SGraph constructor because of Job arg (debug printing)
    // Also checks for non-deterministic payloads
    // Maybe refactor into an SGraph builder util; cf., EGraphBuilderUtil -- but not Visitor-based building (cf. EndpointGraphBuilder), this isn't an AST algorithm
    public SGraph build(Map<Role, EGraph> egraphs, boolean explicit,
                        ProtoName<Global> fullname) throws ScribException {
        this.util.reset();

        SConfig c0 = createInitConfig(egraphs, explicit, fullname);
        SState initState = this.util.newState(c0);
        SScheduler sched;
        if (this.core.config.hasFlag(CoreArgs.SCRIBBLE_UNBOUNDED_BUFFERS)) {  // TODO refactor
            sched = new SSetScheduler();  // TODO factor out (e.g., config factory)
        } else {
            sched = new SDumbScheduler();
        }
        SBuildStateHistory<?> history = sched.newHistory();
        SBuildState init = new SBuildState(initState, history);

        // More efficient to use SConfig instead SState here?
        /*Set<SBuildState> seen = new HashSet<>();
        Set<SBuildState> todo = new LinkedHashSet<>();  // Consider Map<s.id, s> -- faster than full SConfig hash ?*/
        Set<Integer> seen = new HashSet<>();  // Build hashes -- "semantic", no id
        // !!! should be Map<SBuildState, ..semanticHash...> -- compare map.values().contains(candidate.semanticHash)
        // XXX should be Set<SBuildState> -- check none semantics-equal to candidate -- deprecate semanticHash

        Map<Integer, SBuildState> todo = new LinkedHashMap<>();  // Build hash key  // Consider Map<s.id, s> -- faster than full SConfig hash ?

        //todo.add(init);
        todo.put(init.semanticHash(), init);

        for (//int debugCount = 1
                ; !todo.isEmpty(); ) // Compute configs and use util to construct graph, until no more new configs
        {
            /*Iterator<SBuildState> i = todo.iterator();
            SBuildState curr = i.next();
            i.remove();
            seen.add(curr);*/
            Integer i = todo.entrySet().iterator().next().getKey();  // Semantic hash key
            SBuildState curr = todo.get(i);
            todo.remove(i);
            seen.add(curr.semanticHash());

            //System.out.println("1111: " + curr + " ,, " + curr.semanticHash() + "\n" + seen);

			/*if (this.core.config.args.get(CoreArgs.VERBOSE))
			{
				if (debugCount++ % 50 == 0)
				{
					this.core.verbosePrintln(
							"(" + fullname + ") Building global states: " + debugCount);
				}
			}*/

            // Based on dynamic config semantics, not "static" graph edges (cf., super.getActions) -- used to build global model graph
            Map<Role, Set<EAction<StaticActionKind>>> fireable = curr.state.config.getFireable();
            for (Role r : fireable.keySet()) {
                for (EAction<StaticActionKind> a : fireable.get(r)) {

                    if (!sched.canSchedule(curr.history, r, a)) {
                        continue;
                    }
                    /*if (a.isSend()) {
                        sched.add(r, a);
                    } else if (a.isReceive() || a.isDisconnect()) {  // Async
                        sched.clear(a.peer, r);
                    } else if (a.isAccept() || a.isRequest() || a.isClientWrap()
                            || a.isServerWrap()) {  // Sync
                        sched.add(r, a);
                        sched.clear(a.peer, r);
                    }*/

                    // Asynchronous (input/output) actions
                    if (a.isSend() || a.isReceive() || a.isDisconnect()) {
                        Set<SConfig> next = new HashSet<>(curr.state.config.async(r, a));
                        // SConfig.a/sync currently produces a List, but here collapse identical configs for global model (represent non-det "by edges", not "by model states")

                        //Set<SState> succs = this.util.getSuccs(curr.state, a.toStaticGlobal(r), next);  // util.getSuccs constructs the edges
                        Map<Integer, SState> succs = this.util.getSemanticSuccs(curr.state, a.toStaticGlobal(r), next);  // util.getSuccs constructs the edges

                        //for (SState succ : succs) {
                        for (int k : succs.keySet()) {
                            SState succ = succs.get(k);

                            SBuildState bsucc;
                            if (a.isSend()) {
                                bsucc = curr.add(r, a, succ);  // Only ! increases state space (cf. !!, etc)
                            } else if (a.isReceive() || a.isDisconnect()) {
                                //bsucc = curr.clear(r, a, succ);  // Only OK for single cell buffs
                                bsucc = curr.remove(r, a, succ);
                            } else {
                                throw new RuntimeException("Unknown action kind: " + a);
                            }

                            /*if (!seen.contains(bsucc)) {
                                todo.add(bsucc);
                            }*/
                            int sh = bsucc.semanticHash();
                            if (!seen.contains(sh)) {
                                todo.put(sh, bsucc);
                            } else {
                                System.out.println("aaaaaa: " + bsucc + " ,, " + bsucc.semanticHash() + "\n" + seen);
                            }
                        }
                    }
                    // Synchronous (client/server) actions
                    else if (a.isAccept() || a.isRequest() || a.isClientWrap()
                            || a.isServerWrap()) {
                        Set<EAction<DynamicActionKind>> as = fireable.get(a.peer)
                                .stream().map(x -> x.toDynamic()).collect(Collectors.toSet());
                        EAction<DynamicActionKind> abar = a.toDynamicDual(r);
                        if (as != null && as.contains(abar)) {

                            // CHECKME ??
                            as.remove(abar);  // Removes one occurrence

                            SAction<StaticActionKind> aglobal = (a.isRequest() || a.isClientWrap()) // "client" side action
                                    ? a.toStaticGlobal(r)
                                    : abar.toStaticGlobal(a.peer);
                            // CHECKME: edge will be drawn as the connect, but should be read as the sync. of both -- something like "r1, r2: sync" may be more consistent (or take a set of actions as the edge label?)
                            Set<SConfig> next = new HashSet<>(curr.state.config.sync(r, a, a.peer, abar));
                            // SConfig.a/sync currently produces a List, but here collapse identical configs for global model (represent non-det "by edges", not "by model states")

                            //Set<SState> succs = this.util.getSuccs(curr.state, aglobal, next);  // util.getSuccs constructs the edges
                            Map<Integer, SState> succs = this.util.getSemanticSuccs(curr.state, aglobal, next);  // util.getSuccs constructs the edges

                            //for (SState succ : succs) {
                            for (int k : succs.keySet()) {
                                SState succ = succs.get(k);

                                SBuildState bsucc;
                                if (a.isAccept() || a.isRequest() || a.isClientWrap()
                                        || a.isServerWrap()) {
                                    //bsucc = curr.syncClear(r, a.peer, succ);  // Only OK for single cell buffs
                                    bsucc = curr.syncRemove(r, a, succ);
                                } else {
                                    throw new RuntimeException("Unknown action kind: " + a);
                                }

                                /*if (!seen.contains(bsucc)) {
                                    todo.add(bsucc);
                                }*/
                                int sh = bsucc.semanticHash();
                                if (!seen.contains(sh)) {
                                    todo.put(sh, bsucc);
                                }
                            }
                        }
                    } else {
                        throw new RuntimeException("Unknown action kind: " + a);
                    }
                }
            }
        }

        return this.core.config.mf.global.SGraph(
                fullname, this.util.getStates(), initState);
    }
}
