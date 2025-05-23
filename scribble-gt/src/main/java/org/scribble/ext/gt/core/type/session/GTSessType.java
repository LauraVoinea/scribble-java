package org.scribble.ext.gt.core.type.session;


public interface GTSessType {

    // Assumes contractive
    GTSessType unfoldAllImmediateRecs();

    boolean canEquals(Object o);

}
