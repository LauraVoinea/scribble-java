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
package org.scribble.ext.assrt.core.type.formal;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.core.type.kind.ProtoKind;
import org.scribble.core.type.session.SType;
import org.scribble.core.type.session.STypeBase;
import org.scribble.core.visit.STypeAgg;
import org.scribble.core.visit.STypeAggNoThrow;
import org.scribble.ext.assrt.core.type.session.NoSeq;
import org.scribble.util.ScribException;

import java.util.function.Function;
import java.util.stream.Stream;

// SessTypeBase is to SessType as ScribNodeBase is to ScribNode
public abstract class AssrtFormalTypeBase implements AssrtFormalType
{
	@Override
	public int hashCode()
	{
		int hash = 12161;
		//hash = 31*hash + super.hashCode();
		return hash;
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtFormalTypeBase))
		{
			return false;
		}
		return ((AssrtFormalTypeBase) o).canEquals(this);
	}
}
