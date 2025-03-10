package org.scribble.ext.gt.codegen.erlang;

import java.io.IOException;
import java.util.*;

/** Represents an Erlang list (possibly with a tail for an improper list). */
public class ErlList implements ErlTerm {
    private List<ErlTerm> elements;
    private ErlTerm tail;  // optional tail (for improper lists)
    public ErlList(List<ErlTerm> elements) {
        this.elements = new ArrayList<>(elements);
        this.tail = null;
    }
    public ErlList(List<ErlTerm> elements, ErlTerm tail) {
        this.elements = new ArrayList<>(elements);
        this.tail = tail;
    }
    @Override
    public void write(FileWriter w) throws IOException {
        w.write("[");
        for (int i = 0; i < elements.size(); ++i) {
            elements.get(i).write(w);
            if (i < elements.size() - 1) {
                w.write(", ");
            } else if (tail != null) {
                w.write(" | ");
                tail.write(w);
            }
        }
        if (elements.isEmpty() && tail != null) {
            // No head elements, just print the tail (unusual case)
            w.write("| ");
            tail.write(w);
        }
        w.write("]");
    }
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        for (int i = 0; i < elements.size(); ++i) {
            sb.append(elements.get(i).toString());
            if (i < elements.size() - 1) {
                sb.append(", ");
            } else if (tail != null) {
                sb.append(" | ").append(tail.toString());
            }
        }
        if (elements.isEmpty() && tail != null) {
            sb.append("| ").append(tail.toString());
        }
        sb.append("]");
        return sb.toString();
    }
}
