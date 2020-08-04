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
package org.scribble.ext.assrt.core.job;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;

import org.scribble.core.job.Core;
import org.scribble.core.job.CoreContext;
import org.scribble.core.lang.global.GProtocol;
import org.scribble.core.type.kind.Global;
import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.ProtoName;
import org.scribble.core.visit.global.GTypeInliner;
import org.scribble.ext.assrt.core.lang.global.AssrtGProtocol;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.global.AssrtGType;
import org.scribble.ext.assrt.core.visit.gather.AssrtVarEnvGatherer;
import org.scribble.util.RuntimeScribException;

public class AssrtCoreContext extends CoreContext
{
	//protected final Map<ProtoName<Global>, GProtocol> imeds;

	/*// N.B. protos have pruned role decls -- CHECKME: prune args?
	// Mods are preserved
  // Keys are full names
	protected final Map<ProtoName<Global>, GProtocol> inlined = new HashMap<>();

	// CHECKME: rename projis?
	protected final Map<ProtoName<Local>, LProjection> iprojs = new HashMap<>();  // Projected from inlined; keys are full names*/
	
	protected AssrtCoreContext(Core core, Set<GProtocol> imeds)
	{
		super(core, imeds);
	}
	
	@Override
	public Set<ProtoName<Global>> getParsedFullnames()
	{
		return this.imeds.keySet().stream().collect(Collectors.toSet());
	}
	
	@Override
	public AssrtGProtocol getIntermediate(ProtoName<Global> fullname)
	{
		return (AssrtGProtocol) this.imeds.get(fullname);
	}
	
	@Override
	public GProtocol getInlined(ProtoName<Global> fullname) //throws ScribException
	{
		GProtocol inlined = this.inlined.get(fullname);
		if (inlined == null)
		{
			GTypeInliner v = this.core.config.vf.global.GTypeInliner(this.core);  // Factor out?
			inlined = this.imeds.get(fullname).getInlined(v);  // Protocol.getInlined does pruneRecs

			AssrtGProtocol cast = (AssrtGProtocol) inlined;

			// TODO FIXME: here, Map requires distinct annotvars -- but AssrtCore.runGlobalSyntaxWfPasses currently comes after getInlined (runSyntaxTransformPasses)
			List<Entry<AssrtVar, DataName>> res = cast.type.assrtCoreGather(
					new AssrtVarEnvGatherer<Global, AssrtGType>()::visit)
					.collect(Collectors.toList());
			if (res.stream().anyMatch(
					x -> res.stream().anyMatch(y -> x.getKey().equals(y.getKey())
							&& !x.getValue().equals(y.getValue()))))
			{
				throw new RuntimeScribException(
						"[FIXME] Duplicate annot var names with different types: " + res);
				// TODO: refactor with AssrtCore.runGlobalSyntaxWfPasses (which comes later than getInlined pass)
			}

			Map<AssrtVar, DataName> env = res.stream().distinct()
					.collect(Collectors.toMap(x -> x.getKey(), x -> x.getValue()));
			cast.statevars.keySet().forEach(x -> env.put(x, new DataName("int")));  // FIXME "int"
			/*AssrtCoreGType tmp = cast.type;  // Cf. AssrtCoreSConfig.getInitRecAssertCheck
			while (tmp instanceof AssrtCoreGRec)
			{
				AssrtCoreGRec foo = (AssrtCoreGRec) tmp;
				env.putAll(cast.statevars);
				tmp = foo.body;
			}*/

			AssrtGType body = cast.type.disamb((AssrtCore) this.core, env);
			LinkedHashMap<AssrtVar, AssrtAFormula> svars = new LinkedHashMap<>();
			cast.statevars.entrySet().forEach(x -> svars.put(x.getKey(),
					(AssrtAFormula) x.getValue().disamb(env)));  // Unnecessary, disallow mutual var refs?
			AssrtBFormula ass = (AssrtBFormula) cast.assertion.disamb(env);  // TODO: throw ScribbleException, for WF errors
			inlined = new AssrtGProtocol(inlined.getSource(), inlined.mods,
					inlined.fullname, inlined.roles, inlined.params,
					body, cast.statevars, ass, cast.located);

			addInlined(fullname, inlined);
		}

		return inlined;
	}
	
	@Override
	protected void addInlined(ProtoName<Global> fullname, GProtocol g)
	{
		this.inlined.put(fullname, g);
	}
}
