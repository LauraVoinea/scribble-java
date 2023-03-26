package org.scribble.ext.gt.core.model.global;

import org.scribble.core.model.ActionKind;
import org.scribble.core.model.endpoint.EModelFactory;
import org.scribble.core.model.global.SModelFactory;
import org.scribble.ext.gt.core.model.global.action.GTSNewTimeout;

public interface GTSModelFactory extends SModelFactory {

    <A extends ActionKind> GTSNewTimeout<A> SNewTimeout(int c, int n);
}
