package org.scribble.ext.gt.codegen.erlang;

import java.io.IOException;
import java.util.*;

/** Represents an Erlang tuple. */
public class ErlTuple implements ErlTerm {
    private List<ErlTerm> elements;
    public ErlTuple(List<ErlTerm> elements) {
        this.elements = new ArrayList<>(elements);
    }
    @Override
    public void write(FileWriter w) throws IOException {
        w.write("{");
        for (int i = 0; i < elements.size(); ++i) {
            elements.get(i).write(w);
            if (i < elements.size() - 1) {
                w.write(", ");
            }
        }
        w.write("}");
    }
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("{");
        for (int i = 0; i < elements.size(); ++i) {
            sb.append(elements.get(i).toString());
            if (i < elements.size() - 1) {
                sb.append(", ");
            }
        }
        sb.append("}");
        return sb.toString();
    }
}
