package org.scribble.ext.gt.core.type.name;

import org.scribble.core.type.name.Op;

import java.util.Collections;
import java.util.Set;
import java.util.stream.Collectors;

public class GTOp extends Op {

    public static final String EXPLICIT_COMMIT = "*";

    public final Set<String> annots;

    public GTOp(String text) {
        super(text);
        this.annots = Collections.emptySet();
    }

    public GTOp(Set<String> annots, String text) {
        super(text);
        this.annots = annots.stream().collect(Collectors.toSet());
    }
}
