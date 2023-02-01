package org.scribble.core.model;

import org.scribble.core.type.kind.AbstractKind;

public class DynamicActionKind extends AbstractKind implements ActionKind {

    public static final DynamicActionKind KIND = new DynamicActionKind();

    protected DynamicActionKind() {
        super("Static");
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof DynamicActionKind)) {
            return false;
        }
        return ((DynamicActionKind) o).canEqual(this);
    }

    @Override
    public boolean canEqual(Object o) {
        return o instanceof DynamicActionKind;
    }
}

