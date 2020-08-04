package org.scribble.ext.assrt.job;

import org.scribble.core.job.ScribNames;

// Mutable
public class AssrtScribNames extends ScribNames
{
	@Override
	public String toString()
	{
		return "(modules=" + this.modules + ", types=" + this.data + ", sigs="
				+ this.sigs + ", globals=" + this.globals + ", locals=" + this.locals;
		//+ ", " + this.asserts + ")";
	}
}
