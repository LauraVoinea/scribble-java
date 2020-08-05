package org.scribble.ext.assrt.cli;

import java.util.Map;

import org.scribble.cli.CLFlag;
import org.scribble.cli.CLFlags;

public class AssrtCLFlags extends CLFlags
{
	// Unique flags
	public static final String ASSRT_CORE_FLAG = "-assrt";

	public static final String ASSRT_CORE_NATIVE_Z3_FLAG = "-z3";
	public static final String ASSRT_CORE_BATCH_Z3_FLAG = "-batch";  // subsume -z3?
	
	// Non-unique flags
	public static final String ASSRT_CORE_PROJECT_FLAG = "-assrt-project";  // TODO: return inlined projection (currently deprecated)
	/*public static final String ASSRT_CORE_EFSM_FLAG = "-assrt-fsm";
	public static final String ASSRT_CORE_EFSM_PNG_FLAG = "-assrt-fsmpng";*/
	

	@Override
	protected Map<String, CLFlag> getFlags()
	{
		Map<String, CLFlag> flags = super.getFlags();

		// Unique; barrier irrelevant
		flags.put(ASSRT_CORE_FLAG, 
				new CLFlag(ASSRT_CORE_FLAG, 0, true, false, false, ""));
		flags.put(ASSRT_CORE_NATIVE_Z3_FLAG, 
				new CLFlag(ASSRT_CORE_NATIVE_Z3_FLAG, 0, true, false, false, ""));
		flags.put(ASSRT_CORE_BATCH_Z3_FLAG, 
				new CLFlag(ASSRT_CORE_BATCH_Z3_FLAG, 0, true, false, false, ""));

		// Non-unique, no barrier
		flags.put(ASSRT_CORE_PROJECT_FLAG, new CLFlag(ASSRT_CORE_PROJECT_FLAG, 1,
				false, true, false, "Missing role arg: "));
		
		return flags;
	}
}

