package org.scribble.ext.gt.core.model.global;

import org.scribble.core.model.global.SModelFactory;
import org.scribble.ext.gt.core.model.global.action.GTSNewTimeout;

public interface GTSModelFactory extends SModelFactory {

   GTSNewTimeout SNewTimeout(int c, int n);
}
