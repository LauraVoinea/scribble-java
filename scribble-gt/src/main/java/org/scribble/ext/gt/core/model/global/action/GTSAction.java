package org.scribble.ext.gt.core.model.global.action;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.ext.gt.core.model.local.GTEModelFactory;
import org.scribble.ext.gt.core.model.local.action.GTEAction;

public interface GTSAction { //extends SAction<DynamicActionKind> {  // No such interface...

    GTEAction project(GTEModelFactory mf);

    default SAction<DynamicActionKind> toDynamic() {
        throw new RuntimeException("Shouldn't get here: " + this);
    }

    int getC();

    int getN();
}
