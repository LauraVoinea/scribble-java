package org.scribble.ext.gt.codegen;

import org.scribble.core.model.StaticActionKind;
import org.scribble.core.model.endpoint.EStateKind;
import org.scribble.core.model.endpoint.actions.ERecv;
import org.scribble.core.model.endpoint.actions.ESend;
import org.scribble.core.type.name.GProtoName;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.gt.core.model.local.GTEState;

import java.util.*;
import java.util.stream.Collectors;

public class GTApiGen {

    public static final String OSTATE_TYPE = "GTOStateChan";
    public static final String ISTATE_TYPE = "GTIStateChan";
    public static final String BSTATE_TYPE = "GTBStateChan";
    public static final String END_TYPE = "GTEnd";

    public String generate(GProtoName proto, Role r, GTEState init) {
        LinkedHashMap<Integer, GTEState> reach = new LinkedHashMap<>();
        reach.put(init.id, init);
        reach.putAll(init.getReachableStates());

        Map<Integer, String> names = new LinkedHashMap<>();
        reach.values().forEach(x -> names.put(x.id, makeName(proto, r, x)));

        //GProtoName proto = inlined.fullname.getSimpleName();
        List<GIndentable> membs = new LinkedList<>();

        // !!! FIXME
        //membs.add(new GPackage("tmp.scratch.scratch07." + getProtoPackageName(proto)));
        //membs.add(new GImport("tmp.scratch.scratch07.eventactor", List.of("Actor", "Done", "Session")));

        /*List<Role> peers = //inlined.roles.stream().filter(x -> !x.equals(r)).sorted((o1, o2) -> Comparator.<String>naturalOrder().compare(o1.toString(), o2.toString())).toList();
                inlined.roles.stream().filter(x -> !x.equals(r)).toList();
        membs.add(generateActorTrait(names, proto, r, peers, ss.get(0)));*/

        for (GTEState s : reach.values()) {

            EStateKind kind = s.getStateKind();
            switch (kind) {
                case OUTPUT -> membs.add(generateOutputState(names, proto, r, s));
                case UNARY_RECEIVE -> membs.add(generateUnaryInputState(names, proto, r, s));
                case POLY_RECIEVE -> membs.addAll(generateBranchState(names, proto, r, s));
                case TERMINAL -> membs.add(generateTerminalState(names, proto, r, s));
                default -> throw new RuntimeException("Unexpected state kind: " + kind);
            }
        }

        //return generateTop(proto, r) + "\n" + res;
        return membs.stream().map(GIndentable::toString).collect(Collectors.joining("\n\n"));

    }

    /* ... */

    protected GIndentable generateOutputState(Map<Integer, String> names, GProtoName proto, Role r, GTEState s) {
        List<String> mods = List.of();
        String name = getStateTypeName(names, s);

        GConstructor ctor = new GConstructor(mods, name, List.of(), List.of(), "");

        Optional<String> ext = Optional.of(OSTATE_TYPE);
        List<GMethod> methods = s.getDetActions().stream()
                                 .map(x -> generateSend(names, (ESend<StaticActionKind>) x, s.getDetSuccessor(x))).toList();
        return new GClass(mods, name, List.of(ctor), List.of(), methods, ext, List.of());
    }

    // ESend<StaticActionKind> from GTFsmConstructor -- GTESend is only used dynamically?
    protected GMethod generateSend(Map<Integer, String> names, ESend<?> a, GTEState succ) {
        List<String> mods = List.of();
        String name = "send_" + a.peer + "_" + a.mid;
        List<GParam> params = List.of(new GParam(List.of(), getPayType(a.payload), "x"));
        String ret = getStateTypeName(names, succ);
        String body = "return new " + ret + "();";  // TODO

        return new GMethod(mods, name, List.of(), params, ret, body);
    }

    protected static String getPayType(Payload pay) {
        if (pay.elems.size() != 1) {
            throw new RuntimeException("TODO: " + pay);
        }
        return pay.elems.get(0).toString();
    }


    /* ... */

    protected GIndentable generateUnaryInputState(Map<Integer, String> names, GProtoName proto, Role r, GTEState s) {
        List<String> mods = List.of();
        String name = getStateTypeName(names, s);

        GConstructor ctor = new GConstructor(mods, name, List.of(), List.of(), "");

        Optional<String> ext = Optional.of(ISTATE_TYPE);
        ERecv<StaticActionKind> a = (ERecv<StaticActionKind>) s.getDetActions().get(0);
        List<GMethod> methods = List.of(generateReceive(names, a, s.getDetSuccessor(a)));
        return new GClass(mods, name, List.of(ctor), List.of(), methods, ext, List.of());
    }

    // ESend<StaticActionKind> from GTFsmConstructor -- GTESend is only used dynamically?
    protected GMethod generateReceive(Map<Integer, String> names, ERecv<?> a, GTEState succ) {
        List<String> mods = List.of();
        String name = "receive_" + a.peer + "_" + a.mid;
        List<GParam> params = List.of(new GParam(List.of(), "Buf<" + getPayType(a.payload) + ">", "x"));
        String ret = getStateTypeName(names, succ);
        String body = "return new " + ret + "();";  // TODO

        return new GMethod(mods, name, List.of(), params, ret, body);
    }


    /* ... */

    protected List<GIndentable> generateBranchState(Map<Integer, String> names, GProtoName proto, Role r, GTEState s) {
        List<String> mods = List.of();
        String name = getStateTypeName(names, s);

        GConstructor ctor = new GConstructor(mods, name, List.of(), List.of(), "");

        Optional<String> ext = Optional.of(BSTATE_TYPE);
        List<GMethod> methods = List.of(generateBranch(names, proto, r, s));
        List<GIndentable> res = new LinkedList<>();
        res.add(new GClass(mods, name, List.of(ctor), List.of(), methods, ext, List.of()));
        res.addAll(generateCases(names, proto, r, s));
        return res;
    }

    protected GMethod generateBranch(Map<Integer, String> names, GProtoName proto, Role r, GTEState s) {
        List<String> mods = List.of();
        String name = "branch_" + s.getDetActions().get(0).peer;
        List<GParam> params = List.of();
        String ret = getCasesInterfaceName(names, s);
        String body = "return null;  // TODO";  // TODO

        return new GMethod(mods, name, List.of(), params, ret, body);
    }

    protected List<GIndentable> generateCases(Map<Integer, String> names, GProtoName proto, Role r, GTEState s) {
        List<GIndentable> res = new LinkedList<>();
        res.add(new GInterface(List.of(), getCasesInterfaceName(names, s), List.of(), List.of(), List.of()));
        res.addAll(s.getDetActions().stream().map(x -> generateCase(names, proto, r, s, (ERecv<StaticActionKind>) x)).toList());
        return res;
    }

    protected GIndentable generateCase(Map<Integer, String> names, GProtoName proto, Role r, GTEState s, ERecv<StaticActionKind> a) {
        List<String> mods = List.of();
        String name = getCaseName(names, s, a);

        GConstructor ctor = new GConstructor(mods, name, List.of(), List.of(), "");

        Optional<String> ext = Optional.of(ISTATE_TYPE);
        List<String> impls = List.of(getCasesInterfaceName(names, s));
        List<GMethod> methods = List.of(generateReceive(names, a, s.getDetSuccessor(a)));
        return new GClass(mods, name, List.of(ctor), List.of(), methods, ext, impls);
    }

    protected String getCasesInterfaceName(Map<Integer, String> names, GTEState s) {
        return getStateTypeName(names, s) + "_Cases";
    }

    protected String getCaseName(Map<Integer, String> names, GTEState s, ERecv<StaticActionKind> a) {
        return getStateTypeName(names, s) + "_" + a.mid;
    }

    /* ... */

    protected GIndentable generateTerminalState(Map<Integer, String> names, GProtoName proto, Role r, GTEState s) {
        List<String> mods = List.of();
        String name = getStateTypeName(names, s);
        List<GConstructor> ctors = List.of();
        Optional<String> ext = Optional.of(END_TYPE);
        List<GMethod> methods = List.of(); //generateFinish(r));
        return new GClass(mods, name, ctors, List.of(), methods, ext, List.of());
    }


    /* ... */

    protected static String getStateTypeName(Map<Integer, String> names, GTEState s) {
        return names.get(s.id);
    }

    private int count = 1;

    protected String makeName(GProtoName proto, Role r, GTEState s) {
        return s.getActions().isEmpty()
               ? "End"
               : // "S" + this.count++;
               proto + "_" + r + "_" + this.count++;
    }


    /* ... */

    interface GIndentable {
        String toString(String pref);
    }

    class GPackage implements GIndentable {
        public final String name;

        public GPackage(String name) {
            this.name = name;
        }

        @Override
        public String toString() {
            return toString("");
        }

        @Override
        public String toString(String pref) {
            return pref + "package " + this.name;
        }
    }

    class GImport implements GIndentable {
        public final String pref;  // no trailing "."
        public final List<String> names;  // non-empty

        public GImport(String pref, List<String> names) {
            this.pref = pref;
            this.names = List.copyOf(names);
        }

        @Override
        public String toString() {
            return toString("");
        }

        @Override
        public String toString(String pref) {
            return pref + "import " + this.pref + "."
                    + (this.names.size() == 1 ? this.names.get(0) : "{" + String.join(", ", this.names) + "}");
        }
    }

    class GTrait implements GIndentable {
        public final List<String> mods;
        public final String name;
        public final List<GMethod> methods;
        public final List<String> supers;

        public GTrait(List<String> mods, String name, List<String> supers, List<GMethod> methods) {
            this.mods = List.copyOf(mods);
            this.name = name;
            this.supers = List.copyOf(supers);
            this.methods = List.copyOf(methods);
        }

        @Override
        public String toString() {
            return toString("");
        }

        @Override
        public String toString(String pref) {
            return pref + (this.mods.isEmpty() ? "" : String.join(" ", this.mods) + " ") + "trait " + this.name + (this.supers.isEmpty() ? "" : " extends " + String.join(", ", supers))
                    + (this.methods.isEmpty()
                       ? ""
                       : " {\n\n" + pref + this.methods.stream().map(x -> x.toString(pref + "\t")).collect(Collectors.joining("\n\n")) + "\n}");
        }
    }

    class GField extends GParam implements GIndentable {

        public final String init;

        public GField(List<String> mods, String type, String name, String init) {
            super(mods, type, name);
            this.init = init;
        }

        @Override
        public String toString(String pref) {
            return pref + super.toString() + " = " + this.init;
        }
    }

    class GClass extends GClassOrCompanion {
        public GClass(List<String> mods, String name, List<GConstructor> ctors,
                      List<GField> fields, List<GMethod> methods, Optional<String> ext, List<String> impls) {
            super("class", mods, name, ctors, fields, methods, ext, impls);
        }
    }

    /*class GObject extends GClassOrCompanion {
        public GObject(List<String> mods, String name, List<GParam> params,
                       List<GField> fields, List<GMethod> methods, List<String> supers) {
            super("object", mods, name, params, fields, methods, supers);
        }
    }*/

    abstract class GClassOrCompanion implements GIndentable {
        public final String kind;
        public final List<String> mods;
        public final String name;
        //public final List<GParam> params;
        public final List<GConstructor> ctors;
        public final List<GField> fields;
        public final List<GMethod> methods;
        public final Optional<String> ext;
        public final List<String> impls;

        public GClassOrCompanion(String kind, List<String> mods, String name,
                                 //List<GParam> params,
                                 List<GConstructor> ctors,
                                 List<GField> fields, List<GMethod> methods, Optional<String> ext, List<String> impls) {
            this.kind = kind;
            this.mods = List.copyOf(mods);
            this.name = name;
            this.ctors = List.copyOf(ctors);
            this.fields = List.copyOf(fields);
            this.methods = List.copyOf(methods);
            this.ext = ext;
            this.impls = List.copyOf(impls);
        }

        @Override
        public String toString() {
            return toString("");
        }

        @Override
        public String toString(String pref) {
            return pref + (this.mods.isEmpty() ? "" : String.join(" ", this.mods) + " ") + this.kind + " " + this.name
                    + (this.ext.isEmpty() ? "" : " extends " + String.join(", ", this.ext.get()))
                    + (this.impls.isEmpty() ? "" : " implements " + String.join(", ", this.impls))
                    + " {\n"
                    + (this.fields.isEmpty()
                       ? ""
                       : " \n" + pref + this.fields.stream().map(x -> x.toString(pref + "\t")).collect(Collectors.joining("\n")) + "\n")
                    + (this.ctors.isEmpty()
                       ? ""
                       : " \n" + pref + this.ctors.stream().map(x -> x.toString(pref + "\t")).collect(Collectors.joining("\n\n")) + "\n")
                    + (this.methods.isEmpty()
                       ? ""
                       : " \n" + pref + this.methods.stream().map(x -> x.toString(pref + "\t")).collect(Collectors.joining("\n\n")) + "\n")
                    + "}";
        }
    }

    class GInterface implements GIndentable {
        public final List<String> mods;
        public final String name;
        public final List<GField> fields;
        public final List<GMethodSig> sigs;
        public final List<String> exts;

        public GInterface(List<String> mods, String name,
                          List<GField> fields, List<GMethodSig> sigs, List<String> exts) {
            this.mods = List.copyOf(mods);
            this.name = name;
            this.fields = List.copyOf(fields);
            this.sigs = List.copyOf(sigs);
            this.exts = List.copyOf(exts);
        }

        @Override
        public String toString() {
            return toString("");
        }

        @Override
        public String toString(String pref) {
            return pref + (this.mods.isEmpty() ? "" : String.join(" ", this.mods) + " ") + "interface " + this.name
                    + (this.exts.isEmpty() ? "" : " implements " + String.join(", ", this.exts))
                    + " {\n"
                    + (this.fields.isEmpty()
                       ? ""
                       : " \n" + pref + this.fields.stream().map(x -> x.toString(pref + "\t")).collect(Collectors.joining("\n")) + "\n")
                    + (this.sigs.isEmpty()
                       ? ""
                       : " \n" + pref + this.sigs.stream().map(x -> x.toString(pref + "\t")).collect(Collectors.joining(";\n\n")) + "\n")
                    + "}";
        }
    }

    class GConstructor implements GIndentable {
        public final List<String> mods;
        public final String name;
        public final List<GTParam> tParams;
        public final List<GParam> params;
        public final String body;

        public GConstructor(List<String> mods, String name, List<GTParam> tParams, List<GParam> params, String body) {
            this.mods = List.copyOf(mods);
            this.name = name;
            this.tParams = List.copyOf(tParams);
            this.params = List.copyOf(params);
            this.body = body;
        }

        @Override
        public String toString() {
            return toString("");
        }

        @Override
        public String toString(String pref) {
            return pref + (this.mods.isEmpty() ? "" : String.join(" ", this.mods) + " ") + this.name + (this.tParams.isEmpty() ? "" : "[" + this.tParams.stream().map(GTParam::toString).collect(Collectors.joining(", ")) + "]") + "(" + this.params.stream().map(GParam::toString).collect(Collectors.joining(", ")) + ") {"
                    + "\n" + pref + "\t" + this.body.replaceAll("\\n", "\n" + pref + "\t")
                    + "\n" + pref + "}";
        }
    }

    class GMethod implements GIndentable {
        // TODO GMethodSig
        public final List<String> mods;
        public final String name;
        public final List<GTParam> tParams;
        public final List<GParam> params;
        public final String ret;

        public final String body;

        public GMethod(List<String> mods, String name, List<GTParam> tParams, List<GParam> params, String ret, String body) {
            this.mods = List.copyOf(mods);
            this.name = name;
            this.tParams = List.copyOf(tParams);
            this.params = List.copyOf(params);
            this.ret = ret;
            this.body = body;
        }

        @Override
        public String toString() {
            return toString("");
        }

        @Override
        public String toString(String pref) {
            return pref + (this.mods.isEmpty() ? "" : String.join(" ", this.mods) + " ") + this.ret + " " + this.name + (this.tParams.isEmpty() ? "" : "[" + this.tParams.stream().map(GTParam::toString).collect(Collectors.joining(", ")) + "]") + "(" + this.params.stream().map(GParam::toString).collect(Collectors.joining(", ")) + ") {"
                    + "\n" + pref + "\t" + this.body.replaceAll("\\n", "\n" + pref + "\t")
                    + "\n" + pref + "}";
        }
    }

    // No trailing `;`
    class GMethodSig implements GIndentable {
        public final List<String> mods;
        public final String name;
        public final List<GTParam> tParams;
        public final List<GParam> params;
        public final String ret;

        public GMethodSig(List<String> mods, String name, List<GTParam> tParams, List<GParam> params, String ret) {
            this.mods = List.copyOf(mods);
            this.name = name;
            this.tParams = List.copyOf(tParams);
            this.params = List.copyOf(params);
            this.ret = ret;
        }

        @Override
        public String toString() {
            return toString("");
        }

        @Override
        public String toString(String pref) {
            return pref + (this.mods.isEmpty() ? "" : String.join(" ", this.mods) + " ") + this.ret + " " + this.name + (this.tParams.isEmpty() ? "" : "[" + this.tParams.stream().map(GTParam::toString).collect(Collectors.joining(", ")) + "]") + "(" + this.params.stream().map(GParam::toString).collect(Collectors.joining(", ")) + ")";
        }
    }

    class GTParam {
        final String name;
        final String upper;

        public GTParam(String name, String upper) {
            this.name = name;
            this.upper = upper;
        }

        @Override
        public String toString() {
            return this.name + " <: " + this.upper;
        }
    }

    class GParam {
        final List<String> mods;
        final String type;
        final String name;

        public GParam(List<String> mods, String type, String name) {
            this.mods = List.copyOf(mods);
            this.type = type;
            this.name = name;
        }

        @Override
        public String toString() {
            return (this.mods.isEmpty() ? "" : String.join(" ", this.mods) + " ")
                    + this.type + " " + this.name;
        }
    }

}
