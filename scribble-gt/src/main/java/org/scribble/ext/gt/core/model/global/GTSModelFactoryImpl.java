package org.scribble.ext.gt.core.model.global;

import org.scribble.core.model.ModelFactory;
import org.scribble.core.model.global.SModelFactoryImpl;
import org.scribble.ext.gt.core.model.global.action.GTSNewTimeout;

public class GTSModelFactoryImpl extends SModelFactoryImpl implements GTSModelFactory {

    public GTSModelFactoryImpl(ModelFactory mf) {
        super(mf);
    }

    @Override
    public GTSNewTimeout SNewTimeout(int c, int n) {
        return new GTSNewTimeout(c, n);
    }
}
