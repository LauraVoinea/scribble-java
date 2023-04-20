package org.scribble.ext.ea.core.runtime;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.Delta;

public interface EAThread {

    default boolean isIdle() { return false; }

    void type(@NotNull GammaState gamma, @NotNull Delta delta);

    boolean canEquals(Object o);
}
