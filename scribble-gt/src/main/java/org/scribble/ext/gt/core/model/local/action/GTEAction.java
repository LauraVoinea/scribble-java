package org.scribble.ext.gt.core.model.local.action;

import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.action.GTSAction;

public interface GTEAction {

    int getC();

    int getN();

    GTSAction mirror(GTSModelFactory mf, Role self);
}
