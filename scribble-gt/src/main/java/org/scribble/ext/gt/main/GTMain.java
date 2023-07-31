package org.scribble.ext.gt.main;

import org.scribble.ast.AstFactory;
import org.scribble.ast.Module;
import org.scribble.core.job.CoreArgs;
import org.scribble.core.type.name.ModuleName;
import org.scribble.del.DelFactory;
import org.scribble.ext.gt.core.job.GTJob;
import org.scribble.ext.gt.del.GTDelFactoryImpl;
import org.scribble.ext.gt.parser.GTScribAntlrWrapper;
import org.scribble.job.Job;
import org.scribble.main.Main;
import org.scribble.main.resource.locator.ResourceLocator;
import org.scribble.parser.ScribAntlrWrapper;
import org.scribble.util.ScribException;
import org.scribble.util.ScribParserException;

import java.nio.file.Path;
import java.util.Map;

public class GTMain extends Main {
    //public final Solver solver; //= Solver.NATIVE_Z3;
    //public final boolean batching;

	/*// Load main module from file system
	public GTMain(ResourceLocator locator, Path mainpath,
				  Map<CoreArgs, Boolean> args)
			throws ScribException, ScribParserException
	{
		super(locator, mainpath, args);
	}*/

    // Duplicated from AssrtMain
    // Load main module from file system
    public GTMain(ResourceLocator locator, Path mainpath, Map<CoreArgs, Boolean> args)
            throws ScribException, ScribParserException {
        super(locator, mainpath, args);
    }

    public GTMain(String inline, Map<CoreArgs, Boolean> args) throws ScribParserException, ScribException {
        super(inline, args);
    }

    @Override
    protected GTJob newJob(Map<ModuleName, Module> parsed, Map<CoreArgs, Boolean> args,
                           ModuleName mainFullname, AstFactory af, DelFactory df)
            throws ScribException {
        return new GTJob(mainFullname, args, parsed, af, df);
    }

    @Override
    protected ScribAntlrWrapper newAntlr(DelFactory df) {
        return new GTScribAntlrWrapper(df);
    }
	
	/*@Override
	protected AstFactory newAstFactory(ScribAntlrWrapper antlr)
	{
		return new AssrtAstFactoryImpl(antlr.tokens, antlr.df);
	}*/

    @Override
    protected DelFactory newDelFactory() {
        return new GTDelFactoryImpl();
    }

	/*@Override
	public AssrtJob newJob(Map<ModuleName, Module> parsed, CoreArgs args,
			ModuleName mainFullname, AstFactory af, DelFactory df)
			throws ScribException
	{
		return new AssrtJob(mainFullname, (AssrtCoreArgs) args, parsed, af, df);
	}*/
}
