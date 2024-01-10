package org.scribble.ext.gt.core.type.session;


public interface GTSessType {

    // Assumes contractive
    GTSessType unfoldAllOnce();

    boolean canEquals(Object o);

}
