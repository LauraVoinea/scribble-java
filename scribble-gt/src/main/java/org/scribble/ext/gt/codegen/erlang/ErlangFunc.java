package org.scribble.ext.gt.codegen.erlang;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class ErlangFunc {

    //public final List<String> mods;
    public final String name;
    //public final List<GTParam> tParams;
    public final List<String> params;
    public final Optional<String> when;
    public final String body;

    public ErlangFunc(String name, List<String> params, String body) {
        this(name, params, Optional.empty(), body);
    }

    protected ErlangFunc(String name, List<String> params, Optional<String> when, String body) {
        this.name = name;
        this.params = Collections.unmodifiableList(new ArrayList<>(params));
        this.when = when;
        this.body = body;
    }

    public ErlangFunc(String name, List<String> params, String when, String body) {
        this(name, params, Optional.of(when), body);
    }

    public String toString(String pref) {
        String indent = pref + "    ";
        return pref + name
                + "(" + this.params.stream().collect(Collectors.joining(", ")) + ")"
                + (this.when.isPresent() ? "when " + this.when.get() + " " : "")
                + "->\n" + indent
                + this.body.replaceAll("\\n", "\n" + indent);
    }

    @Override
    public String toString() {
        return toString("");
    }
}
