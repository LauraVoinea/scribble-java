package org.scribble.ext.gt.codegen.erlang;

import java.io.IOException;

/** Represents an integer literal in the Erlang AST. */
public class ErlInteger implements ErlTerm {
    private final int value;

    public ErlInteger(int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }

    @Override
    public void write(FileWriter w) throws IOException {
        // Write the integer value as a string.
        w.write(String.valueOf(value));
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!(obj instanceof ErlInteger)) return false;
        ErlInteger other = (ErlInteger) obj;
        return this.value == other.value;
    }

    @Override
    public int hashCode() {
        return Integer.hashCode(value);
    }
}
