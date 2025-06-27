package org.scribble.ext.gt.core.job;

import org.scribble.ast.AstFactory;
import org.scribble.ast.Module;
import org.scribble.core.job.Core;
import org.scribble.core.job.CoreArgs;
import org.scribble.core.lang.global.GProtocol;
import org.scribble.core.type.name.ModuleName;
import org.scribble.core.type.session.STypeFactory;
import org.scribble.core.type.session.global.GTGTypeFactoryImpl;
import org.scribble.core.type.session.local.LTypeFactoryImpl;
import org.scribble.del.DelFactory;
import org.scribble.ext.gt.cli.GTCLFlags;
import org.scribble.ext.gt.cli.GTCommandLine2;
import org.scribble.job.Job;
import org.scribble.util.ScribException;

import java.util.Map;
import java.util.Set;

public class GTJob extends Job {

    public GTJob(ModuleName mainFullname, Map<CoreArgs, Boolean> args,
                 Map<ModuleName, Module> parsed, AstFactory af, DelFactory df)
            throws ScribException {
        super(mainFullname, args, parsed, af, df);
    }

    @Override
    protected Core newCore(ModuleName mainFullname, Map<CoreArgs, Boolean> args,
                           Set<GProtocol> imeds, STypeFactory tf) {
        return new GTCore(mainFullname, args, imeds, tf);
    }

    @Override
    protected STypeFactory newSTypeFactory() {
        return new STypeFactory(
                new GTGTypeFactoryImpl(), new LTypeFactoryImpl());
    }

    // !!! Workaround for role annots
    @Override
    public void runPasses() throws ScribException {
        /*verbosePrintPass("Starting Job passes on:");
        for (ModuleName fullname : this.context.getFullModuleNames()) {
            verbosePrintln(this.context.getModule(fullname).toString());
        }*/

        boolean explicitObserverLeftCommits = GTCommandLine2.ARGS.stream().anyMatch(
                x -> x.left.equals(GTCLFlags.GT_EXPLICIT_OBSERVER_LEFT_COMMITS));
        runVisitorPassOnAllModules(this.config.vf.DelDecorator(this));

        if (!explicitObserverLeftCommits) {
            runVisitorPassOnAllModules(this.config.vf.NameDisambiguator(this));  // Includes validating names used in subprotocol calls...
        }
    }
}
