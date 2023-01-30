package org.scribble.ext.gt.core.job;

import org.scribble.core.job.Core;
import org.scribble.core.job.CoreArgs;
import org.scribble.core.lang.global.GProtocol;
import org.scribble.core.model.ModelFactory;
import org.scribble.core.model.endpoint.EModelFactoryImpl;
import org.scribble.core.type.name.ModuleName;
import org.scribble.core.type.session.STypeFactory;
import org.scribble.ext.gt.core.model.global.GTSModelFactoryImpl;

import java.util.Set;

public class GTCore extends Core {

    public GTCore(ModuleName mainFullname, CoreArgs args,
                  Set<GProtocol> imeds, STypeFactory tf) {
        super(mainFullname, args, imeds, tf);
    }

    @Override
    protected ModelFactory newModelFactory() {
        return new ModelFactory(EModelFactoryImpl::new,
                GTSModelFactoryImpl::new);
    }
}
