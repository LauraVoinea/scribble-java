/**
 * Copyright 2008 The Scribble Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package org.scribble.ext.assrt.core.visit;

import org.scribble.core.job.Core;
import org.scribble.core.type.kind.ProtoKind;
import org.scribble.core.type.session.Choice;
import org.scribble.core.type.session.Continue;
import org.scribble.core.type.session.DirectedInteraction;
import org.scribble.core.type.session.DisconnectAction;
import org.scribble.core.type.session.Do;
import org.scribble.core.type.session.Recursion;
import org.scribble.core.visit.STypeInliner;
import org.scribble.ext.assrt.core.type.session.AssrtCoreSType;
import org.scribble.ext.assrt.core.type.session.NoSeq;

public abstract class AssrtCoreSTypeInliner<K extends ProtoKind, 
			B extends AssrtCoreSType<K, B>>
		extends STypeInliner<K, NoSeq<K>>
{

	public AssrtCoreSTypeInliner(Core core)
	{
		super(core);
	}
	
	@Override
	public AssrtCoreSType<K, B> visitContinue(Continue<K, NoSeq<K>> n)
	{
		throw new RuntimeException("Deprecated for " + getClass() + ":\n" + n);
	}

	@Override
	public AssrtCoreSType<K, B> visitChoice(Choice<K, NoSeq<K>> n)
	{
		throw new RuntimeException("Deprecated for " + getClass() + ":\n" + n);
	}

	@Override
	public AssrtCoreSType<K, B> visitDirectedInteraction(
			DirectedInteraction<K, NoSeq<K>> n)
	{
		throw new RuntimeException("Deprecated for " + getClass() + ":\n" + n);
	}

	@Override
	public AssrtCoreSType<K, B> visitDisconnect(DisconnectAction<K, NoSeq<K>> n)
	{
		throw new RuntimeException("Deprecated for " + getClass() + ":\n" + n);
	}

	@Override
	public AssrtCoreSType<K, B> visitDo(Do<K, NoSeq<K>> n)
	{
		throw new RuntimeException("Deprecated for " + getClass() + ":\n" + n);
	}

	@Override
	public AssrtCoreSType<K, B> visitRecursion(Recursion<K, NoSeq<K>> n)
	{
		throw new RuntimeException("Deprecated for " + getClass() + ":\n" + n);
	}

	// Param "hardcoded" to B (cf. Seq, or SType return) -- this visitor pattern depends on B for Choice/Recursion/etc reconstruction
	@Override
	public NoSeq<K> visitSeq(NoSeq<K> n)
	{
		throw new RuntimeException("Deprecated for " + getClass() + ":\n" + n);
	}
	
}
