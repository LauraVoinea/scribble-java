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
import org.scribble.core.lang.SubprotoSig;
import org.scribble.core.type.kind.Global;
import org.scribble.core.type.name.RecVar;
import org.scribble.ext.assrt.core.type.session.global.AssrtCoreGType;
import org.scribble.ext.assrt.core.visit.AssrtCoreSTypeInliner;

public class AssrtCoreGTypeInliner extends AssrtCoreSTypeInliner<Global, 
		AssrtCoreGType>
{
	public AssrtCoreGTypeInliner(Core core)
	{
		super(core);
	}

	// For public
	@Override
	public boolean hasSig(SubprotoSig sig)
	{
		return super.hasSig(sig);
	}
	
	// For public
	@Override
	public void popSig()
	{
		super.popSig();
	}

	@Override
	public RecVar getInlinedRecVar(RecVar rv)
	{
		return super.getInlinedRecVar(rv);
	}
}
