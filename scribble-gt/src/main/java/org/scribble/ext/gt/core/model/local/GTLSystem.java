package org.scribble.ext.gt.core.model.local;

import org.scribble.core.type.name.Role;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class GTLSystem {

    public final Map<Role, GTLConfig> configs;

    public GTLSystem(Map<Role, GTLConfig> configs) {
        this.configs = Collections.unmodifiableMap(new HashMap<>(configs));
    }


    @Override
    public String toString() {
        return this.configs.toString();
    }

    /* ... */

    @Override
    public int hashCode() {
        int hash = 49123;
        hash = 31 * hash + this.configs.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) { return true; }
        if (obj == null || !(obj instanceof GTLSystem)) { return false; }
        GTLSystem them = (GTLSystem) obj;
        return this.configs.equals(them.configs);
    }
}
