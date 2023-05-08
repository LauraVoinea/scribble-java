package org.scribble.ext.gt.core.model.global.action;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.global.actions.SAction;

public interface GTSAction { //extends SAction {  // No interface...

    default SAction<DynamicActionKind> toDynamic() {
        throw new RuntimeException("Shouldn't get here: " + this);
    }

    int getC();

    int getN();
}
