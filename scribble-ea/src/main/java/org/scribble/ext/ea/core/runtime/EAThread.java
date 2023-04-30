package org.scribble.ext.ea.core.runtime;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.Delta;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;

public interface EAThread {

    default boolean isIdle() { return false; }

    Either<Exception, Tree<String>> type(@NotNull GammaState gamma, @NotNull Delta delta);

    boolean canEquals(Object o);
}
