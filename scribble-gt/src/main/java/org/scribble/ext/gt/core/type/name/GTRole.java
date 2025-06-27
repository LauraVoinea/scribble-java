package org.scribble.ext.gt.core.type.name;

import org.scribble.core.type.name.Role;

import java.util.Collections;
import java.util.Set;
import java.util.stream.Collectors;

public class GTRole extends Role {

    public static final String FAILED_ANNOT = "@failed";

    public final Set<String> annots;

    public GTRole(String text) {
        super(text);
        this.annots = Collections.emptySet();
    }

    public GTRole(Set<String> annots, String text) {
        super(text);
        this.annots = annots.stream().collect(Collectors.toSet());
    }
}
