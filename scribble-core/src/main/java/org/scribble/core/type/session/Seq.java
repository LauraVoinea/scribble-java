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

package org.scribble.core.type.session;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.core.type.kind.ProtoKind;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

// Seq is a SType: an "ad hoc sequential composition", and convenient for, e.g., STypeVisitor's return type
public interface Seq<K extends ProtoKind, B extends Seq<K, B>>
        extends SType<K, B> {

    // Re. return type, could make SType subclasses take themself as another param, but not worth it
    List<? extends SType<K, B>> getElements();

    // Corresponds to all getters (incl. super)
    B reconstruct(CommonTree source, List<? extends SType<K, B>> elems);

    @Override
    default <T> Stream<T> gather(Function<SType<K, B>, Stream<T>> f) {
        return getElements().stream().flatMap(x -> x.gather(f));
    }

    default boolean isEmpty() {
        return getElements().isEmpty();
    }
}

	
	
	
	
	
	
	
	
	
	
	
	

	/*@Override
	public Set<Role> getRoles()
	{
		return this.elems.stream().flatMap(x -> x.getRoles().stream())
				.collect(Collectors.toSet());
	}

	@Override
	public Set<MessageId<?>> getMessageIds()
	{
		return this.elems.stream().flatMap(x -> x.getMessageIds().stream())
				.collect(Collectors.toSet());
	}
	
	@Override
	public Set<RecVar> getRecVars()
	{
		return this.elems.stream().flatMap(x -> x.getRecVars().stream())
				.collect(Collectors.toSet());
	}

	@Override
	public B getInlined(STypeInliner v)
	{
		CommonTree source = getSource(); // CHECKME: or empty source?
		List<SType<K, B>> elems = new LinkedList<>();
		for (SType<K, B> e : this.elems)
		{
			SType<K, B> e1 = e.getInlined(v);
			if (e1 instanceof Seq<?, ?>)
			{
				elems.addAll(((Seq<K, B>) e1).elems); // Inline Seq's returned by Do.getInlined
			}
			else
			{
				elems.add(e1);
			}
		}
		return reconstruct(source, elems);
	}

	@Override
	public B unfoldAllOnce(STypeUnfolder<K> u)
	{
		CommonTree source = getSource();
		List<SType<K, B>> elems = new LinkedList<>();
		for (SType<K, B> e : this.elems)
		{
			SType<K, B> e1 = e.unfoldAllOnce(u);
			if (e1 instanceof Seq<?, ?>)
			{
				elems.addAll(((Seq<K, B>) e1).elems);
			}
			else
			{
				elems.add(e1);
			}
		}
		return reconstruct(source, elems);
	}
		
	@Override
	public List<ProtocolName<K>> getProtoDependencies()
	{
		return this.elems.stream().flatMap(x -> x.getProtoDependencies().stream())
				.distinct().collect(Collectors.toList());
	}

	@Override
	public List<MemberName<?>> getNonProtoDependencies()
	{
		return this.elems.stream()
				.flatMap(x -> x.getNonProtoDependencies().stream()).distinct()
				.collect(Collectors.toList());
	}

	@Override
	public B substitute(Substitutions subs)
	{
		List<? extends SType<K, B>> elems = this.elems.stream()
				.map(x -> x.substitute(subs)).collect(Collectors.toList());
		return reconstruct(getSource(), elems);
	}

	@Override
	public B pruneRecs()
	{
		List<SType<K, B>> elems = new LinkedList<>();
		for (SType<K, B> e : this.elems)
		{
			SType<K, B> e1 = (SType<K, B>) e.pruneRecs();
			if (e1 instanceof Seq<?, ?>)  // cf. Recursion::pruneRecs
			{
				elems.addAll(((Seq<K, B>) e1).getElements());  // Handles empty Seq case
			}
			else
			{
				elems.add(e1);
			}
		}
		return reconstruct(getSource(), elems);
	}
	*/
	
	/*@Override
	public <T> T aggregate(STypeAgg<K, B, T> v) throws ScribException
	{
		@SuppressWarnings("unchecked")
		B cast = (B) this;
		return v.visitSeq(cast);
	}
	
	@Override
	public <T> T aggregateNoEx(STypeAggNoEx<K, B, T> v)
	{
		@SuppressWarnings("unchecked")
		B cast = (B) this;
		return v.visitSeq(cast);
	}

	// Override STypeBase.visitWith for B return
	// Alternatively: call v.visitSeq directly, bypasses generic cast
	@Override
	public B visitWith(STypeVisitor<K, B> v) throws ScribException
	{
		@SuppressWarnings("unchecked")
		B cast = (B) this;  // CHECKME: OK as long as G/LSeq specify themselves as B param
		return v.visitSeq(cast);
	}

	// Override STypeBase.visitWithNoEx for B return
	@Override
	public B visitWithNoEx(STypeVisitorNoEx<K, B> v)
	{
		@SuppressWarnings("unchecked")
		B cast = (B) this;
		return v.visitSeq(cast);
	}
	*/
