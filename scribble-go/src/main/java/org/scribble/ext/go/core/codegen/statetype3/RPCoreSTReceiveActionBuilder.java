package org.scribble.ext.go.core.codegen.statetype3;

import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.scribble.codegen.statetype.STReceiveActionBuilder;
import org.scribble.codegen.statetype.STStateChanApiBuilder;
import org.scribble.ext.go.core.model.endpoint.action.RPCoreEAction;
import org.scribble.ext.go.core.type.RPIndexedRole;
import org.scribble.ext.go.core.type.RPInterval;
import org.scribble.ext.go.main.GoJob;
import org.scribble.model.endpoint.EState;
import org.scribble.model.endpoint.actions.EAction;
import org.scribble.type.name.MessageSigName;

public class RPCoreSTReceiveActionBuilder extends STReceiveActionBuilder
{

	@Override
	public String getActionName(STStateChanApiBuilder api, EAction a)
	{
		return RPCoreSTStateChanApiBuilder.getGeneratedIndexedRoleName(((RPCoreEAction) a).getPeer())
				+ "_" + RPCoreSTApiGenConstants.RP_GATHER_METHOD_PREFIX  // FIXME: make unary Receive special case
				+ "_" + a.mid;
	}

	@Override
	public String buildArgs(STStateChanApiBuilder api, EAction a)
	{
		RPCoreSTStateChanApiBuilder rpapi = (RPCoreSTStateChanApiBuilder) api;
		if (a.mid.isOp())
		{
			return IntStream.range(0, a.payload.elems.size()) 
					.mapToObj(i -> RPCoreSTApiGenConstants.GO_CROSS_RECEIVE_METHOD_ARG + i + " []"
							+ rpapi.getPayloadElemTypeName(a.payload.elems.get(i)))
					.collect(Collectors.joining(", "));
		}
		else //if (a.mid.isMessageSigName())
		{
			return RPCoreSTApiGenConstants.GO_CROSS_RECEIVE_METHOD_ARG + "0 []"
					+ rpapi.getExtName((MessageSigName) a.mid);
		}
	}

	@Override
	public String buildBody(STStateChanApiBuilder api, EState curr, EAction a, EState succ)
	{
		if(a.payload.elems.size() > 1)
		{
			throw new RuntimeException("[param-core] TODO: " + a);
		}

		RPCoreSTStateChanApiBuilder rpapi = (RPCoreSTStateChanApiBuilder) api;
		RPIndexedRole peer = (RPIndexedRole) a.peer;
		RPInterval d = peer.intervals.iterator().next();
		if (peer.intervals.size() > 1)
		{
			throw new RuntimeException("[rp-core] TODO: " + a);
		}

		String sEpRecv = RPCoreSTApiGenConstants.GO_IO_METHOD_RECEIVER + "." + RPCoreSTApiGenConstants.GO_SCHAN_ENDPOINT
				+ "." + RPCoreSTApiGenConstants.GO_MPCHAN_SESSCHAN; /*+ "." //+ RPCoreSTApiGenConstants.GO_CONNECTION_MAP
				+ RPCoreSTApiGenConstants.GO_MPCHAN_FORMATTER_MAP
				+ "[\"" +  peer.getName() + "\"]";*/

		String res = "";
		if (a.payload.elems.size() > 1)
		{
			throw new RuntimeException("[param-core] [TODO] payload size > 1: " + a);
		}

		if (((GoJob) rpapi.job).noCopy)
		{
			/*res += "data := make([]" + a.payload.elems.get(0) + ", len(b))\n"
				+ "for i := 0; i < len(b); i++ {\n"
				+ "data[i] = *b[i].(*" + a.payload.elems.get(0) + ")\n"
				+ "}\n"
				+ "*arg0 = reduceFn0(data)\n";*/
			throw new RuntimeException("[rp-core] TODO: -nocopy: " + a);  // FIXME: currently missing from sender side?
		}
		else
		{
			String start = rpapi.generateIndexExpr(d.start);
			res += //"var err error\n"
					  "for i := " + start + ";"
					+ " i <= " + rpapi.generateIndexExpr(d.end) + "; i++ {\n";

			// For payloads -- FIXME: currently hardcoded for exactly one payload

			String errorField = RPCoreSTApiGenConstants.GO_IO_METHOD_RECEIVER + "."
                                + RPCoreSTApiGenConstants.GO_SCHAN_ENDPOINT + "."
                                + "_" + rpapi.getSuccStateChanName(succ) + "."
                                + RPCoreSTApiGenConstants.GO_MPCHAN_ERR;

			if (a.mid.isOp())
			{
				//if (!a.mid.toString().equals("")) // HACK FIXME?  // Now redundant, -param-api checks mid starts uppercase
				{ 
					res += "var lab string\n"
							+ "if " + errorField + " = " + sEpRecv /*+ "[i]"
									+ "." //+ RPCoreSTApiGenConstants.GO_ENDPOINT_READALL + "(" + "&lab" + ")"
											+ RPCoreSTApiGenConstants.GO_FORMATTER_DECODE_STRING + "()"*/
									+ "." + RPCoreSTApiGenConstants.GO_MPCHAN_IRECV + "(\"" + peer.getName() + "\", i, &lab)" 
									+ "; " + errorField + " != nil {\n"
							//+ "log.Fatal(err)\n"
							//+ "return " + rpapi.makeCreateSuccStateChan(succ) + "\n"  // FIXME: disable linearity check for error chan?  Or doesn't matter -- only need to disable completion check?
							+ rpapi.makeReturnSuccStateChan(succ) + "\n"
							+ "}\n";
				}

				if (!a.payload.elems.isEmpty())
				{
					if (a.payload.elems.size() > 1)
					{
						throw new RuntimeException("[rp-core] [-param-api] TODO: " + a);
					}

					Function<String, String> makeReceivePayType = pt -> 
							//+ (extName.startsWith("[]") ? "tmp = make(" + extName + ", len(arg0))\n" : "")  // HACK? for passthru?
							"if " + errorField + " = " + sEpRecv /*+ "[i]"  // FIXME: use peer interval
									+ "." //+ RPCoreSTApiGenConstants.GO_ENDPOINT_READALL + "(&tmp)"
									+ RPCoreSTApiGenConstants.GO_FORMATTER_DECODE_INT + "()"*/
									+ "." + RPCoreSTApiGenConstants.GO_MPCHAN_IRECV + "(\"" + peer.getName() + "\", i, &arg0[i-" + start + "])"
					
							+ "; " + errorField + " != nil {\n"
							//+ "log.Fatal(err)\n"
							//+ "return " + rpapi.makeCreateSuccStateChan(succ) + "\n"  // FIXME: disable linearity check for error chan?  Or doesn't matter -- only need to disable completion check?
							+ rpapi.makeReturnSuccStateChan(succ) + "\n"
							+ "}\n";
							// + "arg0[i-" + start + "] = *(tmp.(*" + pt + "))\n";  // FIXME: doesn't work for gob, pointer decoding seems flattened? ("*" dropped) ...  // Cf. ISend in RPCoreSTSendActionBuilder
							//+ "arg0[i-" + start + "] = tmp.(" + pt + ")\n";  // FIXME: ... but doesn't work for shm
					res += makeReceivePayType.apply(rpapi.getPayloadElemTypeName(a.payload.elems.get(0)));
				}
			}
			else //if (a.mid.isMessageSigName())
			{
				Function<String, String> makeReceiveExtName = extName -> 
							"var tmp " + RPCoreSTApiGenConstants.GO_SCRIBMESSAGE_TYPE + "\n"  // var tmp needed for deserialization -- FIXME?
						+ "if " + errorField + " = " + sEpRecv /*+ "[i]"  // FIXME: use peer interval
								+ RPCoreSTApiGenConstants.GO_FORMATTER_DECODE_INT + "()"*/
								+ "." + RPCoreSTApiGenConstants.GO_MPCHAN_MRECV + "(\"" + peer.getName() + "\", i, &tmp)" 
				
						+ "; " + errorField + " != nil {\n"
						//+ "log.Fatal(err)\n"
						//+ "return " + rpapi.makeCreateSuccStateChan(succ) + "\n"  // FIXME: disable linearity check for error chan?  Or doesn't matter -- only need to disable completion check?
						+ rpapi.makeReturnSuccStateChan(succ) + "\n"
						+ "}\n"
						+ "arg0[i-" + start + "] = *(tmp.(*" + extName + "))\n";  // Cf. ISend in RPCoreSTSendActionBuilder
				res += makeReceiveExtName.apply(rpapi.getExtName((MessageSigName) a.mid));
			}
		
			res += "}\n";
		}
				
		return res + buildReturn(rpapi, curr, succ);
	}
	
	/*protected static String hackGetValues(String t)
	{
		if (t.equals("int"))
		{
			return "util.GetValuesInt";
		}
		else if (t.equals("string"))
		{
			return "util.GetValuesString";
		}
		else if (t.equals("[]byte"))
		{
			return "util.GetValuesBates";
		}
		else
		{
			throw new RuntimeException("[TODO] " + t);
			//return t;
		}
	}*/
}
