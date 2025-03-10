package org.scribble.ext.gt.codegen.erlang;

import java.io.IOException;

/** Common interface for all Erlang AST node classes. */
public interface ErlTerm {
    /** Write AST node to the given FileWriter with proper formatting. */
    void write(FileWriter w) throws IOException;
}