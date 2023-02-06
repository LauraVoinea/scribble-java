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
import org.scribble.job.Job;
import org.scribble.util.ScribException;

import java.util.Map;
import java.util.Set;

public class GTJob extends Job {

    public GTJob(ModuleName mainFullname, CoreArgs args,
                 Map<ModuleName, Module> parsed, AstFactory af, DelFactory df)
            throws ScribException {
        super(mainFullname, args, parsed, af, df);
    }


    @Override
    protected Core newCore(ModuleName mainFullname, CoreArgs args,
                           Set<GProtocol> imeds, STypeFactory tf) {
        return new GTCore(mainFullname, args, imeds, tf);
    }

    @Override
    protected STypeFactory newSTypeFactory() {
        return new STypeFactory(
                new GTGTypeFactoryImpl(), new LTypeFactoryImpl());
    }
}
