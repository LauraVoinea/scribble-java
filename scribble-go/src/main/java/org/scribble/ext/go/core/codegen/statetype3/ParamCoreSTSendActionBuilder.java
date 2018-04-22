package org.scribble.ext.go.core.codegen.statetype3;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.scribble.codegen.statetype.STSendActionBuilder;
import org.scribble.codegen.statetype.STStateChanApiBuilder;
import org.scribble.ext.go.core.model.endpoint.action.ParamCoreEAction;
import org.scribble.ext.go.core.type.ParamRange;
import org.scribble.ext.go.core.type.ParamRole;
import org.scribble.ext.go.type.index.ParamIndexExpr;
import org.scribble.ext.go.type.index.ParamIndexInt;
import org.scribble.ext.go.type.index.ParamIndexVar;
import org.scribble.model.endpoint.EState;
import org.scribble.model.endpoint.actions.EAction;
import org.scribble.type.name.DataType;

public class ParamCoreSTSendActionBuilder extends STSendActionBuilder
{

	@Override
	public String getActionName(STStateChanApiBuilder api, EAction a)
	{
		return //ParamCoreSTApiGenConstants.GO_CROSS_SEND_FUN_PREFIX + "_"
				  "Scatter_"  // FIXME: make unary Send special case
				+ ParamCoreSTStateChanApiBuilder.getGeneratedParamRoleName(((ParamCoreEAction) a).getPeer())
				+ "_" + a.mid;
	}

	@Override
	public String buildArgs(STStateChanApiBuilder apigen, EAction a)
	{
		return IntStream.range(0, a.payload.elems.size()) 
					.mapToObj(i -> ParamCoreSTApiGenConstants.GO_CROSS_SEND_FUN_ARG
							+ i + " []" + ((ParamCoreSTStateChanApiBuilder) apigen).batesHack(a.payload.elems.get(i)) //a.payload.elems.get(i)
							).collect(Collectors.joining(", "));
	}

	@Override
	public String buildBody(STStateChanApiBuilder apigen, EState curr, EAction a, EState succ)
	{
		if(a.payload.elems.size() > 1)
		{
			throw new RuntimeException("[param-core] TODO: " + a);
		}

		List<EAction> as = curr.getActions();
		if (as.size() > 1 && as.stream().anyMatch(b -> b.mid.toString().equals("")))  // HACK
		{
			throw new //ParamCoreException
					RuntimeException("[param-core] Empty labels not allowed in non-unary choices: " + curr.getActions());
		}
		
		String sEpWrite = 
				//s.ep.Write
				 ParamCoreSTApiGenConstants.GO_IO_FUN_RECEIVER + "." + ParamCoreSTApiGenConstants.GO_SCHAN_ENDPOINT 
				 		//+ "." + ParamCoreSTApiGenConstants.GO_ENDPOINT_ENDPOINT
				 				+ ".Ept"
						//+ "." + ParamCoreSTApiGenConstants.GO_ENDPOINT_WRITEALL;
						+ ".Conn";
		/*String sEpProto =
				//"s.ep.Proto"
				ParamCoreSTApiGenConstants.GO_IO_FUN_RECEIVER + "."
					+ ParamCoreSTApiGenConstants.GO_SCHAN_ENDPOINT + "." + ParamCoreSTApiGenConstants.GO_ENDPOINT_PROTO;*/

		ParamRole r = (ParamRole) a.peer;
		ParamRange g = r.ranges.iterator().next();
		Function<ParamIndexExpr, String> foo = e ->
		{
			if (e instanceof ParamIndexInt)
			{
				return e.toString();
			}
			else if (e instanceof ParamIndexVar)
			{
				return ParamCoreSTApiGenConstants.GO_IO_FUN_RECEIVER + "."
					+ ParamCoreSTApiGenConstants.GO_SCHAN_ENDPOINT + ".Params[\"" + e + "\"]";
			}
			else
			{
				throw new RuntimeException("[param-core] TODO: " + e);
			}
		};
				
		// FIXME: single arg  // Currently never true because of ParamCoreSTOutputStateBuilder
		boolean isDeleg = a.payload.elems.stream().anyMatch(pet -> 
				//pet.isGDelegationType()  // FIXME: currently deleg specified by ParmaCoreDelegDecl, not GDelegationElem
				((ParamCoreSTStateChanApiBuilder) apigen).isDelegType((DataType) pet));
		
		String res =
			//st1.Use()
				"j := 0\n"
				+ "for i := " + foo.apply(g.start) + "; i <= "+foo.apply(g.end)+"; i++ {\n"

				//+ ParamCoreSTApiGenConstants.GO_IO_FUN_RECEIVER + "." + ParamCoreSTApiGenConstants.GO_SCHAN_ENDPOINT
				+ (a.mid.toString().equals("") ? "" :  // HACK
					"if err := " + sEpWrite
						//+ "[" +  sEpProto + "." + r.getName() + ".Name()][i]"
						+ "[\"" + r.getName() + "\"][i]"
						+ "." + ParamCoreSTApiGenConstants.GO_ENDPOINT_WRITEALL
						+ "(" //+ sEpProto + "." + r.getName() + ", "
						+ "\"" + a.mid + "\"" + "); err != nil {\n"
						+ "log.Fatal(err)\n"
						+ "}\n")

				//+ ParamCoreSTApiGenConstants.GO_IO_FUN_RECEIVER + "." + ParamCoreSTApiGenConstants.GO_SCHAN_ENDPOINT
				+ ((isDeleg)  //... FIXME: delegation: take pointer?  send underlying ept? -- don't do (multi)"send"?
					?
						"log.Fatal(\"TODO\")\n"
					:
						"if err := " + sEpWrite
							//+ "[" +  sEpProto + "." + r.getName() + ".Name()][i]"
							+ "[\"" + r.getName() + "\"][i]"
							+ "." + ParamCoreSTApiGenConstants.GO_ENDPOINT_WRITEALL
							+ "(" //+ sEpProto + "." + r.getName() + ", "
							+ "arg0[j]" //+ "splitFn0(arg0, i)"  // FIXME: hardcoded arg0
									+ "); err != nil {\n"
							+ "log.Fatal(err)\n"
							+ "}\n"
							+ "j = j+1\n"
					+ "}\n");

			/*for i, v := range pl {
				st1.ept.Conn[Worker][i].Send(a.mid)
				st1.ept.Conn[Worker][i].Send(v)
			}*/
				
		return
					res
				+ buildReturn(apigen, curr, succ);
	}
}
