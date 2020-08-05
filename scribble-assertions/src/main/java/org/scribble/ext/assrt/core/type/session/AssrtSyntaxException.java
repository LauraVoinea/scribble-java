package org.scribble.ext.assrt.core.type.session;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.util.RuntimeScribSyntaxException;

// For parsing errors due to core syntax restrictions (vs. "full" Scribble) -- distinction used for JUnit testing (i.e., to indicate non core syntax that are otherwise valid protocols)
// i.e., should only be thrown by AssrtCoreGProtocolDeclTranslator
// N.B. so should not be used for actual "semantic" WF errors
public class AssrtSyntaxException extends 
		//AntlrSourceException   // N.B. not Scribble/AssrttException -- cf. AssrtCoreTestBase::tests
				// No: don't want GTypeTranslator to throw checked, to avoid throws on Job::getCore 
		RuntimeScribSyntaxException   // CHECKME (AssrtCoreTestBase) -- cf. CommandLine::run
{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;


	public AssrtSyntaxException(String arg0)
	{
		super(arg0);
		// TODO Auto-generated constructor stub
	}

	public AssrtSyntaxException(Throwable arg0)
	{
		super(arg0);
		// TODO Auto-generated constructor stub
	}

	public AssrtSyntaxException(String arg0, Throwable arg1)
	{
		super(arg0, arg1);
		// TODO Auto-generated constructor stub
	}

	public AssrtSyntaxException(String message, Throwable cause, boolean enableSuppression,
			boolean writableStackTrace)
	{
		super(message, cause, enableSuppression, writableStackTrace);
		// TODO Auto-generated constructor stub
	}

	public AssrtSyntaxException(CommonTree blame, String arg0)
	{
		super(blame, arg0);
		// TODO Auto-generated constructor stub
	}

}
