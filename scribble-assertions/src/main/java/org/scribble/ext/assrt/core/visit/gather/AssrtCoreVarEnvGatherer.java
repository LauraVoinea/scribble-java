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
package org.scribble.ext.assrt.core.visit.gather;

import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.scribble.core.type.kind.ProtoKind;
import org.scribble.core.type.name.DataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.AssrtCoreChoice;
import org.scribble.ext.assrt.core.type.session.AssrtCoreSType;

public class AssrtCoreVarEnvGatherer<K extends ProtoKind, 
			B extends AssrtCoreSType<K, B>>
		extends AssrtCoreSTypeGatherer<K, B, Map.Entry<AssrtVar, DataName>>
{
	@Override
	public Stream<Map.Entry<AssrtVar, DataName>> visitChoice(
			AssrtCoreChoice<K, B> n)
	{
		return n.cases.keySet().stream()
				.flatMap(x -> x.pay.stream()
						.collect(Collectors.toMap(y -> y.var, y -> y.data)).entrySet()
						.stream());
	}
}
