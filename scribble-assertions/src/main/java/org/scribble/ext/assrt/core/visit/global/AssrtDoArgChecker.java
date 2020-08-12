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
package org.scribble.ext.assrt.core.visit.global;

import org.scribble.core.job.Core;
import org.scribble.core.type.kind.Global;
import org.scribble.core.type.session.Do;
import org.scribble.core.type.session.SType;
import org.scribble.core.visit.STypeVisitor;
import org.scribble.ext.assrt.core.type.session.NoSeq;
import org.scribble.util.ScribException;

public class AssrtDoArgChecker extends STypeVisitor<Global, NoSeq<Global>>
{
	public final Core core;

	public AssrtDoArgChecker(Core core)
	{
		this.core = core;
	}
	
	/*@Override
	public SType<Global, GSeq> visitChoice(Choice<Global, GSeq> n) throws ScribException
	{
	
	}
	
	@Override
	public SType<Global, GSeq> visitContinue(Continue<Global, GSeq> n) throws ScribException
	{
	}
	
	@Override
	public SType<Global, GSeq> visitDirectedInteraction(
			DirectedInteraction<Global, GSeq> n) throws ScribException
	{
	}
	
	@Override
	public SType<Global, GSeq> visitDisconnect(DisconnectAction<Global, GSeq> n) throws ScribException
	{
	}*/

	@Override
	public final SType<Global, NoSeq<Global>> visitDo(Do<Global, NoSeq<Global>> n)
			throws ScribException
	{
		System.out.println("aaaa: " + n);
		return n;
	}

	/*@Override
	public SType<Global, GSeq> visitRecursion(Recursion<Global, GSeq> n) throws ScribException
	{
	}
	
	@Override
	public GSeq visitSeq(GSeq n)
	{
	}*/
}
