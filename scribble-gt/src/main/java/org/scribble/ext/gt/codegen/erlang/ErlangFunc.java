package org.scribble.ext.gt.codegen.erlang;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public class ErlangFunc {

    //public final List<String> mods;
    public final String name;
    //public final List<GTParam> tParams;
    public final List<String> params;
    public final String body;

    public ErlangFunc(String name, List<String> params, String body) {
        this.name = name;
        this.params = Collections.unmodifiableList(new ArrayList<>(params));
        this.body = body;
    }

    public String toString(String pref) {
        String indent = pref + "    ";
        return pref + name
                + "(" + this.params.stream().collect(Collectors.joining(", ")) + ")"
                + "->\n" + indent
                + this.body.replaceAll("\\n", "\n" + indent);
    }

    @Override
    public String toString() {
        return toString("");
    }
}
