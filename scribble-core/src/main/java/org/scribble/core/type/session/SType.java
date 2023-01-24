package org.scribble.core.type.session;

import org.scribble.core.type.kind.ProtoKind;

public interface SType<K extends ProtoKind, B extends Seq<K, B>>
        extends SVisitable<K, B> {
}
