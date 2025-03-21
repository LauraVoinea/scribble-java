package org.scribble.ext.gt.codegen.erlang;

import java.io.IOException;

/** Represents an Erlang atom (constant). */
public class ErlAtom implements ErlTerm {
    private String value;
    public ErlAtom(String value) {
        this.value = value;
    }
    @Override
    public void write(FileWriter w) throws IOException {
        w.write(toString());
    }
    @Override
    public String toString() {
        if (value == null) {
            return "undefined";
        }
        // If the value is numeric, output as a number literal (no quotes)
        if (value.matches("-?\\d+")) {
            return value;
        }
        // If value is a valid unquoted atom (starts with lowercase, followed by alphanumeric or underscore)
        if (value.matches("[a-z][A-Za-z0-9_]*")) {
            return value;
        }
        // Otherwise, quote the atom and escape any single quotes inside
        String escaped = value.replace("'", "\\'");
        return "'" + escaped + "'";
    }

    public String getValue() {
        return value;
    }
}
