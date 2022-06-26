package org.scribble.ext.ea.core.config;

public interface EAPThreadState {

    boolean canEquals(Object o);

    default boolean isIdle() { return false; }
}
