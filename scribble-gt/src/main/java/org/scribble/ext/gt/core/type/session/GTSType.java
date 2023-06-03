package org.scribble.ext.gt.core.type.session;

public interface GTSType { //<K extends ProtoKind, B extends Seq<K, B>> extends SType<K, B> {

    // Assumes contractive
    GTSType unfoldAllOnce();

    boolean canEquals(Object o);

}
