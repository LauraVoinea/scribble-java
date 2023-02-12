package org.scribble.core.model;

import org.scribble.core.type.kind.AbstractKind;

public class StaticActionKind extends AbstractKind implements ActionKind {

    public static final StaticActionKind KIND = new StaticActionKind();

    protected StaticActionKind() {
        super("Static");
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof StaticActionKind)) {
            return false;
        }
        return ((StaticActionKind) o).canEqual(this);
    }

    @Override
    public boolean canEqual(Object o) {
        return o instanceof StaticActionKind;
    }
}

