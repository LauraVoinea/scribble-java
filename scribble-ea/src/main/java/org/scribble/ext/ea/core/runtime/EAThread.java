package org.scribble.ext.ea.core.runtime;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.Delta;

public interface EAThread {

    default boolean isIdle() { return false; }

    void type(@NotNull Gamma gamma, @NotNull Delta delta);

    boolean canEquals(Object o);
}
