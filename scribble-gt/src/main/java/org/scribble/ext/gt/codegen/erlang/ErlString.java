package org.scribble.ext.gt.codegen.erlang;

import java.io.IOException;

/**
 * Represents an Erlang string literal.
 * This node outputs its value wrapped in double quotes and escapes internal double quotes.
 */
public class ErlString implements ErlTerm {
    private final String value;

    public ErlString(String value) {
        this.value = value;
    }

    @Override
    public void write(FileWriter w) throws IOException {
        // Write the string literal using the toString() representation.
        w.write(toString());
    }

    /**
     * Escapes double quotes within the string.
     */
    private String escape(String s) {
        // Basic escaping: replace " with \"
        return s.replace("\"", "\\\"");
    }

    @Override
    public String toString() {
        return "\"" + escape(value) + "\"";
    }
}
