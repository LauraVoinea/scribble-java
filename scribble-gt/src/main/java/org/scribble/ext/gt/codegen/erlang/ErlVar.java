package org.scribble.ext.gt.codegen.erlang;

import java.io.IOException;

/** Represents an Erlang variable (automatically capitalized). */
public class ErlVar implements ErlTerm {
    private String name;
    public ErlVar(String name) {
        if (name != null && !name.isEmpty()) {
            char first = name.charAt(0);
            // Ensure first character is uppercase or underscore (valid variable start in Erlang)
            if (first != '_' && !Character.isUpperCase(first)) {
                this.name = Character.toUpperCase(first) + name.substring(1);
            } else {
                this.name = name;
            }
        } else {
            this.name = name;
        }
    }
    @Override
    public void write(FileWriter w) throws IOException {
        w.write(name);
    }
    @Override
    public String toString() {
        return name;
    }
}