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
import org.scribble.core.type.name.RecVar;
import org.scribble.core.visit.STypeAgg;
import org.scribble.core.visit.STypeAggNoThrow;
import org.scribble.util.ScribException;

import java.util.function.Function;
import java.util.stream.Stream;

public interface Continue<K extends ProtoKind, B extends Seq<K, B>>
        extends SType<K, B> {

    RecVar getRecVar();

    // Corresponds to all getters (incl. super)
    Continue<K, B> reconstruct(CommonTree source, RecVar recvar);

    @Override
    default <T> T visitWith(STypeAgg<K, B, T> v) throws ScribException {
        return v.visitContinue(this);
    }

    @Override
    default <T> T visitWithNoThrow(STypeAggNoThrow<K, B, T> v) {
        return v.visitContinue(this);
    }

    /*@Override
    default <T> Stream<T> gather(Function<SVisitable<K, B>, Stream<T>> f) {
        return f.apply(this);
    }*/
}

	
	
	
	
	
	
	
	
	
	/*

	@Override
	public SType<K, B> visitWith(STypeVisitor<K, B> v) throws ScribException
	{
		return v.visitContinue(this);
	}

	@Override
	public SType<K, B> visitWithNoEx(STypeVisitorNoEx<K, B> v)
	{
		return v.visitContinue(this);
	}

	@Override
	public Set<Role> getRoles()
	{
		return Collections.emptySet();
	}

	@Override
	public Set<MessageId<?>> getMessageIds()
	{
		return Collections.emptySet();
	}
	
	@Override
	public Set<RecVar> getRecVars()
	{
		return Stream.of(this.recvar).collect(Collectors.toSet());
	}

	@Override
	public Continue<K, B> getInlined(STypeInliner v)
	{
		RecVar rv = v.getInlinedRecVar(this.recvar);
		return reconstruct(getSource(), rv);
	}
	
	@Override
	public List<ProtocolName<K>> getProtoDependencies()
	{
		return Collections.emptyList();
	}

	@Override
	public List<MemberName<?>> getNonProtoDependencies()
	{
		return Collections.emptyList();
	}

	@Override
	public Continue<K, B> substitute(Substitutions subs)
	{
		//return reconstruct(getSource(), this.recvar);
		return this;
	}

	@Override
	public Continue<K, B> pruneRecs()
	{
		//return reconstruct(getSource(), this.recvar);
		return this;
	}
	*/
