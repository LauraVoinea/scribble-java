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
import org.scribble.core.type.session.Choice;
import org.scribble.core.type.session.Recursion;
import org.scribble.core.type.session.SVisitable;
import org.scribble.core.type.session.Seq;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

// For comments, see STypeVisitor
public abstract class STypeVisitorNoThrow<K extends ProtoKind, B extends Seq<K, B>>
        extends STypeAggNoThrow<K, B, SVisitable<K, B>> {

    @Override
    protected final SVisitable<K, B> unit(SVisitable<K, B> n) {
        return n;
    }

    // Should disregard agg for STypeVisitors -- the STypeVisitor pattern is instead to manually reconstruct within each visit[Node]
    @Override
    protected final SVisitable<K, B> agg(SVisitable<K, B> n, Stream<SVisitable<K, B>> ns) {
        throw new RuntimeException("Disregarded for STypeVisitorNoEx: " + n + " ,, " + ns);
    }

    @Override
    public SVisitable<K, B> visitChoice(Choice<K, B> n) {
        List<B> blocks = n.getBlocks().stream().map(x -> visitSeq(x))
                .collect(Collectors.toList());
        return n.reconstruct(n.getSource(), n.getSubject(), blocks);  // Disregarding agg (reconstruction done here)
    }

    @Override
    public SVisitable<K, B> visitRecursion(Recursion<K, B> n) {
        B body = visitSeq(n.getBody());
        return n.reconstruct(n.getSource(), n.getRecVar(), body);  // Disregarding agg (reconstruction done here)
    }

    @Override
    public B visitSeq(B n) {
        List<SVisitable<K, B>> elems = n.getElements().stream()
                .map(x -> x.acceptNoThrow(this))
                .collect(Collectors.toList());
        return n.reconstruct(n.getSource(), elems);  // Disregarding agg (reconstruction done here)
    }

}


















	/*
	public SType<K, B> visitContinue(Continue<K, B> n)
	{
		return n;
	}

	public SType<K, B> visitChoice(Choice<K, B> n)
	{
		List<B> blocks = n.blocks.stream().map(x -> x.visitWithNoEx(this))
				.collect(Collectors.toList());
		return n.reconstruct(n.getSource(), n.subj, blocks);
	}

	public SType<K, B> visitDirectedInteraction(DirectedInteraction<K, B> n)
	{
		//return n.reconstruct(n.getSource(), n.msg, n.src, n.dst);
		return n;
	}

	public SType<K, B> visitDisconnect(DisconnectAction<K, B> n)
	{
		//return n.reconstruct(n.getSource(), n.left, n.right);
		return n;
	}

	public <N extends ProtocolName<K>> SType<K, B> visitDo(Do<K, B, N> n)
	{
		//return n.reconstruct(n.getSource(), n.proto, n.roles, n.args);
		return n;
	}

	public SType<K, B> visitRecursion(Recursion<K, B> n)
	{
		B body = n.body.visitWithNoEx(this);
		return n.reconstruct(n.getSource(), n.recvar, body);
	}

	// "Hardcoded" to B (cf. Seq, or SType return) -- this visitor pattern depends on B for Choice/Recursion/etc reconstruction
	// This means a Visitor that needs to restructure a Seq should handle this within visitSeq
	// E.g., Seq "injection" by inlining and unfolding
	// For this purpose, visited children passed "directly" instead of via a reconstruction (cf. above methods)
	public B visitSeq(B n)
	{
		List<SType<K, B>> elems = n.elems.stream().map(x -> x.visitWithNoEx(this))
				.collect(Collectors.toList());
		return n.reconstruct(n.getSource(), elems);
	}*/


/*@FunctionalInterface
interface STypeVisitorFunction<K extends ProtocolKind, B extends Seq<K, B>, R extends Stream<?>>
{
	R f(SType<K, B> n, STypeVisitor<K, B> v);
}*/
