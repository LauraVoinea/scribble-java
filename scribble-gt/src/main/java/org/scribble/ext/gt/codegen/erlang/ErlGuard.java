package org.scribble.ext.gt.codegen.erlang;

import java.io.IOException;
import java.util.*;

/** Represents an Erlang guard expression (one or more conditions). */
public class ErlGuard implements ErlTerm {
    private List<ErlTerm> conditions = new ArrayList<>();

    public ErlGuard(ErlTerm condition) {
        this.conditions.add(condition);
    }
    public void addCondition(ErlTerm condition) {
        conditions.add(condition);
    }
    @Override
    public void write(FileWriter w) throws IOException {
        for (int i = 0; i < conditions.size(); ++i) {
            conditions.get(i).write(w);
            if (i < conditions.size() - 1) {
                w.write(", ");
            }
        }
    }
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < conditions.size(); ++i) {
            sb.append(conditions.get(i).toString());
            if (i < conditions.size() - 1) {
                sb.append(", ");
            }
        }
        return sb.toString();
    }
}
