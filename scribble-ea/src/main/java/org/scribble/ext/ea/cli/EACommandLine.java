package org.scribble.ext.ea.cli;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.Lexer;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;
import org.scribble.cli.CommandLine;
import org.scribble.cli.CommandLineException;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.local.LTypeFactory;
import org.scribble.core.type.session.local.LTypeFactoryImpl;
import org.scribble.ext.ea.core.runtime.*;
import org.scribble.ext.ea.core.runtime.config.EACActor;
import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.term.EATermFactory;
import org.scribble.ext.ea.core.term.comp.EAComp;
import org.scribble.ext.ea.core.term.comp.EAMLet;
import org.scribble.ext.ea.core.term.expr.EAEHandlers;
import org.scribble.ext.ea.core.term.expr.EAEVar;
import org.scribble.ext.ea.core.term.expr.EAExpr;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.*;
import org.scribble.ext.ea.core.type.value.EAVHandlersType;
import org.scribble.ext.ea.core.type.value.EAVIntType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.parser.antlr.EACalculusLexer;
import org.scribble.ext.ea.parser.antlr.EACalculusParser;
import org.scribble.ext.ea.util.*;
import org.scribble.util.AntlrSourceException;
import org.scribble.util.Pair;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;


//- key point: only "relevant" handlers available at any one time -- cf. prev EDP works (all handlers all the time)

// HERE
//- check getFoo for input side -- generalise for async
//- "unify" handlers and lambdas/recs
//- recursion -- add rec vars
//- separate P vals from terms AST
//- rename pairs->endpoints, triples->"handlers"
//- Need G/L annotations on restr in op sem with updating -- currently EAPSystem.type takes Delta as "manual" arg (add types as EAPSystem fields)
//- Consider async
//- add type method to System, split delta by configs key (endpoints)
//- TEST config typing with handrolled Deltas -- as opposed to T-Session introducing each endpoint to Delta -- also because env splitting is not algorithmic
//- finish testing infer for subset, but then do initiation and top-down register type annot
//- types for subset, initiation+types, if-then-else
//- if-else
//- configs and config exec
//- newAP/spawn/register


		/*%%   p (+) { l1(A) -> p & { ... }, l2(B) -> p (+) { l3(C) -> End } }
%%   let () <=
%%       if rand () then
%%           p ! l1(V);
%%           suspend h1 -- p (+) { l3(C) -> End }
%%       else
%%           p ! l2(W) -- p (+) { l3(C) -> End }
%%   in
%%   -- p (+) { l3(C) -> End }
%%   print("still alive")*/






// Includes assrt-core functionality (all extra args are currently for assrt-core)
public class EACommandLine extends CommandLine {

    static final LinkedHashMap<Pair<EASid, Role>, EAEHandlers> EMPTY_SIGMA = newLinkedMap();

    static final LTypeFactoryImpl LF = new LTypeFactoryImpl();

    static final EATypeFactory TF = EATypeFactory.factory;
    static final EATermFactory MF = EATermFactory.factory;
    static final EARuntimeFactory RF = EARuntimeFactory.factory;

    static final Role A = new Role("A");
    static final Role B = new Role("B");
    static final EASid s = RF.sid("s");
    static final EAPid p1 = RF.pid("p1");
    static final EAPid p2 = RF.pid("p2");
    static final EAEVar x = MF.var("x");

    static final Pair<EASid, Role> sA = Pair.of(s, A);
    static final Pair<EASid, Role> sB = Pair.of(s, B);

    public EACommandLine(String... args) {
        super(args);
    }

    public static void main(String[] args)
            throws CommandLineException, AntlrSourceException {

        //CommandLine.main(args);  // !!! base CommandLine fully bypassed -- No main module used
        eamain();
        //testParser();
    }

    static String log(String log, String name, boolean pass) {
        return log + "\n" + name + (pass ? " Pass" : ConsoleColors.colour(ConsoleColors.RED, " FAIL"));
    }

    private static void tests() {

        //System.out.println(parseV("2 + 3"));

        String log = "";

        Map<String, Function<Boolean, Optional<Exception>>> tests = EAUtil.mapOf();
        tests.put("ex1", EACommandLine::ex1);
        tests.put("ex2", EACommandLine::ex2);

        for (Map.Entry<String, Function<Boolean, Optional<Exception>>> e : tests.entrySet()) {
            String name = e.getKey();
            log = log(log, name, runTest(name, e.getValue(), true));
        }

        System.out.println("\nSummary:" + log);

        /*ex1();
        //ex2();

        /*ex4();
        ex5a();

        ex6();
        ex7();
        ex8();

        ex10();*/
    }

    private static void negtests() {
        LTypeFactoryImpl lf = new LTypeFactoryImpl();

        EATermFactory pf = EATermFactory.factory;
        EARuntimeFactory rf = EARuntimeFactory.factory;
        EATypeFactory tf = EATypeFactory.factory;

        //ex9(lf, pf, rf, tf);
    }

    private static void eamain() {

        //new EACommandLine(args).run();

        /* HERE HERE  // merge rhu1-refactorinterfaces -- i.e., latest scrib-core

        - ...sys/actor/thread typing return Either
        - ...testing summary
        - ...eval Optional/Either return
        - ...Right @NotNull (no Void)
        - ...terms in derivations

        - ...fidelity/coherence -- cf. EAPSystem.annots -- after async
        - ...Optional (cf. canX)
        - ...wild S' in suspend typing
        - ...need (self) handler firing when state satis some condition -- !!! how done in standard actors?
        - ...evolving state types? but tricky if state is config-wide (shared by multiple sessions)

        - add state to return
        - refactor EAPVal as EAPPure -- separate packages for expr/pure -- distinguish actual val from pure
        - separate isGround/isValue -- cf. canBeta for exprs vs. is-stuck
        - fix canBeta contexts and isGround for all exprs
        - tidy foo vs. beta -- cf. some expr foo is just beta (some not, e.g., let)
        */

        //System.out.println(parseV("2 + 3"));

        tests();

    }

    static void ex10() {
        System.out.println("\n--ex10:");

        EAComp lethA = parseM("return 42");
        EAComp lethB = parseM("return 43");

        EACActor cA = RF.config(p1, RF.activeThread(lethA, s, A), EMPTY_SIGMA, MF.intt(1));
        EACActor cB = RF.config(p2, RF.activeThread(lethB, s, B), EMPTY_SIGMA, MF.intt(2));
        System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);

        //--------------

        typeCheckActor(cA, new Delta(newLinkedMap(sA, EALEndType.END)));
        typeCheckActor(cB, new Delta(newLinkedMap(sB, EALEndType.END)));

        // ----

        Delta delta = new Delta(newLinkedMap(sA, EALEndType.END, sB, EALEndType.END));
        EASystem sys = RF.system(LF, delta, newLinkedMap(cA.pid, cA, cB.pid, cB));

        typeAndRun(sys, -1);
    }

    // !!! Not WT -- testing (incompatible) state typing
    // TODO update
    static void ex9(LTypeFactory lf, EATermFactory pf, EARuntimeFactory rf, EATypeFactory tf) {
        Role A = new Role("A");
        Role B = new Role("B");
        EASid s = rf.sid("s");
        EAPid p1 = rf.pid("p1");
        EAPid p2 = rf.pid("p2");

        //---------------

        String in2s = "A?{l2(1).end}";
        String h2s = "Handler (Int, " + in2s + ")";
        EAMLet lethA = (EAMLet) parseM(
                "let h: " + h2s + " <= return handler A {"
                        + "{end} d: Int, l2(x: 1) |-> let w: Bool <= return d < 42 in return ()"
                        + "}"  // role hardcoded -- or state not accessible
                        + " in let g: 1 <= B!l1(h) in B!l2(())"
        );

        //---------------
        // config < A, idle, c[A] |-> let h = ... in ... >
        EATActive tA = rf.activeThread(lethA, s, A);
        LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigmaA = new LinkedHashMap<>();
        EACActor cA = rf.config(p1, tA, sigmaA, pf.factory.intt(0));

        System.out.println();
        String out1s = "B!{l1(" + h2s + ").B!{l2(1). end }}";
        EALOutType out1 = (EALOutType) parseSessionType(out1s);
        LinkedHashMap<Pair<EASid, Role>, EALType> env = new LinkedHashMap<>();
        env.put(new Pair<>(s, A), out1);
        System.out.println("Typing cA: " + cA + " ,, " + env);
        cA.type(new Gamma(), new Delta(env));

        //---------------

        String in1s = "A?{l1(" + h2s + ")." + in2s + "}";
        String hBs = "Handler(Bool, " + in1s + ")";
        EAMLet lethB = (EAMLet) parseM(
                "let h: " + hBs + " <= return handler A { {" + in2s + "} d: Bool, l1(x: " + h2s + ") |-> "
                        + " suspend x false }"  // suspend received x handler, XXX (data) type preservation -- d type Int at A, Bool at B
                        + " in suspend h true"
        );

        //--------------
        // config < B, idle, c[B] |-> let h = ... in ... } >
        EATActive tB = rf.activeThread(lethB, s, B);
        LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigmaB = new LinkedHashMap<>();
        EACActor cB = rf.config(p2, tB, sigmaB, pf.factory.bool(false));

        System.out.println();
        env = new LinkedHashMap<>();
        EALInType in1 = (EALInType) parseSessionType(in1s);
        env.put(new Pair<>(s, B), in1);
        System.out.println("Typing cB: " + cB + " ,, " + env);
        cB.type(new Gamma(), new Delta(env));

        // ----

        System.out.println("\n---");
        System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);
        LinkedHashMap<EAPid, EACActor> cs = new LinkedHashMap<>();
        cs.put(cA.pid, cA);
        cs.put(cB.pid, cB);

        env = new LinkedHashMap<>();
        env.put(new Pair<>(s, A), out1);
        env.put(new Pair<>(s, B), in1);
        System.out.println(env);
        EASystem sys = rf.system(lf, new Delta(env), cs);
        System.out.println(sys);
        sys.type();

        typeAndRun(sys, -1);
        //
    }

    // Various options for B in comments
    static void ex8() {
        System.out.println("\n---ex8:");

        String recXAs = "mu X.B?{l2(1).B!{l1(1).X}, l3(1).end }";
        String out1us = "B!{l1(1)." + recXAs + "}";
        String in2us = "B?{l2(1)." + out1us + ", l3(1).end}";  // unfolding of recXA

        String h2s = "Handler (Int, " + in2us + ")";
        String hts = "{" + recXAs + "} 1 -> " + h2s + "{" + recXAs + "}";
        EAMLet lethA = (EAMLet) parseM(
                "let h: " + hts + " <= return"
                        + "  (rec f { " + recXAs + "} (w1: 1 ): " + h2s + " {" + recXAs + "} . return handler B {"
                        + "    {" + out1us + "} d: Int, l2(w2: 1) |->"
                        + "      let y: 1 <= B!l1(()) in let z : " + h2s + " <= [f ()] in suspend z 42,"
                        + "    {end} d: Int, l3(w3: 1) |-> return d"
                        + "  })"
                        + "in let w3 : 1 <= B!l1(()) in let hh : " + h2s + " <= [h ()] in suspend hh 0");

        String out2s = "A!{l2(1).X, l3(1).end}";
        String in1s = "A?{l1(1)." + out2s + "}";
        String recXBs = "mu X." + in1s;
        String out2mus = "A!{l2(1)." + recXBs + ", l3(1).end}";
        String in1us = "A?{l1(1)." + out2mus + "}";

        // ---

        String h1s = "Handler (Int, " + in1us + ")";
        String htsB = "{" + in1us + "} 1 -> " + h1s + " {" + recXBs + "}";
        EAMLet leth = (EAMLet) parseM(
                "let h: " + htsB + " <= return"
                        + "  (rec f {  " + in1us + "} (w1: 1):" + h1s + "{" + recXBs + "} . return handler A {"
                        + "    {" + out2mus + "} d: Int, l1(w2: 1) "

                        /*//+ " |-> let y: 1 <= A!l2(()) in let z : " + h1s + " <= [f ()] in suspend z 42 })"  // run forever -- old
                        + " |-> let y: 1 <= A!l3(()) in return () })"  // quit straight away*/

                        //+ "     |-> let tmp: Bool <= return d < 0 in "  // quit straight away -- 0
                        + "     |-> let tmp: Bool <= return d < 42 in "  // quit after one -- 42
                        //+ "     |-> let tmp: Bool <= return d < 43 in "  // run forever -- change run(-1) below

                        + "        if tmp then let y: 1 <= A!l2(()) in let z : " + h1s + " <= [f ()] in suspend z 42"
                        + "        else let y: 1 <= A!l3(()) in return d"

                        + "  }) "
                        + "in let hh : " + h1s + " <= [h ()] in suspend hh 0");

        EACActor cA = RF.config(p1, RF.activeThread(lethA, s, A), EMPTY_SIGMA, MF.factory.intt(0));
        EACActor cB = RF.config(p2, RF.activeThread(leth, s, B), EMPTY_SIGMA, MF.factory.intt(0));
        System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);

        //---------------

        EALOutType out1u = (EALOutType) parseSessionType(out1us);
        EALRecType recXB = (EALRecType) parseSessionType(recXBs);

        /*lethA.type(new GammaState(EAVIntType.INT), out1u);
        leth.type(new GammaState(EAVIntType.INT), recXB);*/

        typeCheckActor(cA, new Delta(newLinkedMap(sA, out1u)));
        typeCheckActor(cB, new Delta(newLinkedMap(sB, recXB)));

        // ----

        Delta delta = new Delta(newLinkedMap(sA, out1u, sB, recXB));
        EASystem sys = RF.system(LF, delta, newLinkedMap(cA.pid, cA, cB.pid, cB));
        typeAndRun(sys, -1, true);  // quit straight away or after one
        //typeAndRun(sys, 100);  // run forever
    }

    static void ex7() {
        System.out.println("\n---ex7:");

        String recXAs = "mu X.B?{l2(1).B!{l1(1).X, l4(1).end}, l3(1).end }";
        String out1us = "B!{l1(1)." + recXAs + ", l4(1).end}";
        String in2us = "B?{l2(1)." + out1us + ", l3(1).end}";  // unfolding of recXA
        String h2s = "Handler (Int, " + in2us + ")";

        String hts = "{" + recXAs + "} 1 -> " + h2s + " {" + recXAs + "}";
        EAMLet lethA = (EAMLet) parseM(
                "let h: " + hts + " <= return"
                        + "  (rec f { " + recXAs + "} (w1: 1 ):" + h2s + "{" + recXAs + "} . return handler B {"
                        + "    {" + out1us + "} d: Int, l2(w2: 1) |-> let y: 1 <= B!l4(()) in return d,"
                        + "    {end} d: Int, l3(w3: 1) |-> return d"
                        + "  })"
                        + "in let w3 : 1 <= B!l1(()) in let hh : " + h2s + " <= [h ()] in suspend hh 0");

        String recXBs = "mu X.A?{l1(1).A!{l2(1).X, l3(1).end}, l4(1).end}";
        String out2mus = "A!{l2(1)." + recXBs + ", l3(1).end}";
        String in1us = "A?{l1(1)." + out2mus + ", l4(1).end}";
        String h1s = "Handler (Int, " + in1us + ")";

        String htsB = "{" + in1us + "} 1 -> " + h1s + " {" + recXBs + "}";
        EAMLet leth = (EAMLet) parseM(
                "let h: " + htsB + " <= return"
                        + "  (rec f{  " + in1us + "} (w1: 1):" + h1s + "{" + recXBs + "} . return handler A {"
                        + "    {" + out2mus + "} d: Int, l1(w2: 1) |->"
                        + "      let y: 1 <= A!l2(()) in let z : " + h1s + " <= [f ()] in suspend z 42,"
                        + "    {end} d: Int, l4(w4: 1) |-> return d"
                        + "  })"
                        + "in let hh : " + h1s + " <= [h ()] in suspend hh 0");

        EACActor cA = RF.config(p1, RF.activeThread(lethA, s, A), EMPTY_SIGMA, MF.factory.intt(0));
        EACActor cB = RF.config(p2, RF.activeThread(leth, s, B), EMPTY_SIGMA, MF.factory.intt(0));
        System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);

        //---------------

        EALOutType out1u = (EALOutType) parseSessionType(out1us);
        EALRecType recXB = (EALRecType) parseSessionType(recXBs);

        /*lethA.type(new GammaState(EAVIntType.INT), out1u);
        leth.type(new GammaState(EAVIntType.INT), recXB);*/

        typeCheckActor(cA, new Delta(newLinkedMap(sA, out1u)));
        typeCheckActor(cB, new Delta(newLinkedMap(sB, recXB)));

        // ----

        Delta delta = new Delta(newLinkedMap(sA, out1u, sB, recXB));
        EASystem sys = RF.system(LF, delta, newLinkedMap(cA.pid, cA, cB.pid, cB));
        typeAndRun(sys, -1);
    }

    private static void ex6() {
        System.out.println("\n---ex6");

        EALOutType out1 = (EALOutType) parseSessionType("B!{l1(Bool).end}");
        EAComp sendAB = parseM("let x: Bool <= return 2 < 2 in if x then B!l1(x) else B!l1(x)");

        EALInType in1 = (EALInType) parseSessionType("A?{l1(Bool).end}");
        EAEHandlers hsB = (EAEHandlers) parseV("handler A { {end} z1: 1, l1(x: Bool) |-> return 42 }");

        EACActor cA = RF.config(p1, RF.activeThread(sendAB, s, A), EMPTY_SIGMA, MF.unit());
        EACActor cB = RF.config(p2, RF.idle(), newLinkedMap(sB, hsB), MF.intt(0));
        System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);

        // ---

        //System.out.println("Typing eA: " + out1 + " ,, " + sendAB.type(new GammaState(EAVIntType.INT), out1));

        typeCheckActor(cA, new Delta(newLinkedMap(sA, out1)));
        typeCheckActor(cB, new Delta(newLinkedMap(sB, in1)));

        // ---

        Delta delta = new Delta(newLinkedMap(sA, out1, sB, in1));
        EASystem sys = RF.system(LF, delta, newLinkedMap(cA.pid, cA, cB.pid, cB));
        typeAndRun(sys, -1);
    }

    private static void ex5a() {
        System.out.println("\n--ex5");

        String recXAs = "mu X . B?{l2(1).B!{l1(1).X}, l3(1).end}";
        String out1us = "B!{l1(1)." + recXAs + "}";
        String in2us = "B?{l2(1)." + out1us + ", l3(1).end}";
        String h2s = "Handler(Int, " + in2us + ")";

        String ftAs = "{" + in2us + "} 1 -> " + h2s + " {" + recXAs + "}";
        EAMLet lethA = (EAMLet) parseM(
                "let h : " + ftAs + " <= return"
                        + "  (rec f {" + in2us + "} (w1 :1): " + h2s + " {" + recXAs + "} . return handler B {"
                        + "    {" + out1us + "} d: Int, l2(w2: 1) |->"
                        + "      let y: 1 <= B!l1(()) in let z : " + h2s + " <= [f ()] in suspend z 42, "
                        + "    {end} d: Int, l3(w2: 1) |-> return d"
                        + "  }) "
                        + "in let w1: 1 <= B!l1(()) in let hh: " + h2s + " <= [h ()] in suspend hh 42");

        String recXBs = "mu X . A?{ l1(1) . A!{ l2(1) . X, l3(1).end }}";
        String out2us = "A!{l2(1) . " + recXBs + ", l3(1) . end }";
        String in1us = "A?{l1(1) . " + out2us + "}";
        String h1s = "Handler(Int, " + in1us + ")";

        String fts = "{" + in1us + "} 1 -> " + h1s + "{" + recXBs + "}";
        EAMLet leth = (EAMLet) parseM(
                "let h: " + fts + " <= return"
                        + "  (rec f {" + in1us + "} (w1: 1): " + h1s + " {" + recXBs + "} . return handler A {"
                        + "    {" + out2us + "} d: Int, l1(w2: 1) |-> let y: 1 <= A!l3(()) in return d"
                        + "  }) "
                        + "in let hh: " + h1s + " <= [h ()] in suspend hh 43");

        EACActor cA = RF.config(p1, RF.activeThread(lethA, s, A), EMPTY_SIGMA, MF.intt(0));
        EACActor cB = RF.config(p2, RF.activeThread(leth, s, B), EMPTY_SIGMA, MF.intt(0));
        System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);

        // ---

        EALOutType out1u = (EALOutType) parseSessionType(out1us);
        EALRecType recXB = (EALRecType) parseSessionType(recXBs);

        /*lethA.type(new GammaState(EAVIntType.INT), out1u);
        leth.type(new GammaState(EAVIntType.INT), recXB);*/

        typeCheckActor(cA, new Delta(newLinkedMap(sA, out1u)));
        typeCheckActor(cB, new Delta(newLinkedMap(sB, recXB)));

        // ----

        Delta delta = new Delta(newLinkedMap(sA, out1u, sB, recXB));
        EASystem sys = RF.system(LF, delta, newLinkedMap(cA.pid, cA, cB.pid, cB));
        typeAndRun(sys, -1);
    }

    // XXX DEBUG loop -- cf. ex5a OK
    private static void ex5() {
        System.out.println("\n---ex5:");

        String recXAs = "mu X . B?{l2(1).B!{l1(1).X}, l3(1).end}";
        String out1us = "B!{l1(1).mu X . " + recXAs + "}";
        String in2us = "B?{l2(1)." + out1us + ", l3(1).end}";
        String h2s = "Handler(Int, " + in2us + ")";

        String ftAs = "{" + in2us + "} 1 -> " + h2s + "{" + recXAs + "}";
        EAMLet lethA = (EAMLet) parseM(
                "let h : " + ftAs + " <= return rec f {" + in2us + "} (w1 :1) : " + h2s + " {" + recXAs
                        + "} . return handler B { {" + out1us + "} z2: Int, l2(w2: 1) |-> let y :1 <= B!l1(())"
                        + "in let z : " + h2s + " <= [f ()] in suspend z 42 ,"
                        + "{end} z3: Int, l3(w2: 1) |-> return z3 } "
                        + "in let w1 :1 <= B!l1(()) in let hh: " + h2s + " <= [h ()] in suspend hh 42");

        System.out.println(lethA);

        String out2s = "A!{ l2(1) . X, l3(1).end }";
        String in1s = "A?{ l1(1) . " + out2s + " }";
        String recXBs = "mu X . " + in1s;
        EALRecType recXB = (EALRecType) parseSessionType(recXBs);

        String out2mus = "A!{l2(1) . " + recXBs + ", l3(1) . end }";

        String out2us = out2mus;
        String in1us = "A?{l1(1) . " + out2us + "}";

        String h1s = "Handler(Int, " + in1us + ")";
        EAVHandlersType h1 = (EAVHandlersType) parseA(h1s);

        String fts = "{" + in1us + "} 1 -> " + h1s + "{" + recXBs + "}";
        EAMLet leth = (EAMLet) parseM(
                //"let h : {A?{l1(1).A!{l2(1).mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}, l3(1).end}}}1 -> Handler(A?{l1(1).A!{l2(1).mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}, l3(1).end}}) {mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}} <= return rec f { A?{l1(1).A!{l2(1).mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}, l3(1).end}}} (w1 :1) :Handler(A?{l1(1).A!{l2(1).mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}, l3(1).end}}) {mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}} . return handler A { l1(w2: 1) : A!{l2(1).mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}, l3(1).end} |-> let y :1 <= A!l3(()) in return () } in let hh :Handler(A?{l1(1).A!{l2(1).mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}, l3(1).end}}) <= [h ()] in suspend hh");
                "let h: " + fts + " <= return rec f {" + in1us + "} (w1 :1): " + h1s + "{" + recXBs
                        + "} . return handler A { {" + out2us + "} z1: Int, l1(w2: 1) |-> let y :1 <= A!l3(()) in return z1 } "
                        + "in let hh: " + h1s + " <= [h ()] in suspend hh 42");

        // config < A, idle, c[A] |-> let h = ... in ... >
        EATActive tA = RF.activeThread(lethA, s, A);
        LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigmaA = new LinkedHashMap<>();
        /*LinkedHashMap<Pair<EAPSid, Role>, Integer> stateA = new LinkedHashMap<>();
        stateA.put(new Pair<>(s, A), 0);
        EAPConfig cA = rf.config(p1, tA, sigmaA, stateA);*/
        EACActor cA = RF.config(p1, tA, sigmaA, MF.intt(0));

        // config < B, idle, c[B] |-> let h = ... in ... >
        System.out.println();

		/*LinkedHashMap<EAName, EAValType> map = new LinkedHashMap<>();
		map.put(x, tf.val.unit());
		Gamma gamma = new Gamma(map, new LinkedHashMap<>());
		System.out.println("Typing hB: " + hsB1.type(gamma));

		LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaB = new LinkedHashMap<>();
		sigmaB.put(new Pair<>(s, B), hsB1);  // !!! TODO make sigma concrete, e.g., for typing
		EAPConfig cB = rf.config(p2, rf.idle(), sigmaB);*/

        EATActive tB = RF.activeThread(leth, s, B);
        LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigmaB = new LinkedHashMap<>();
        /*LinkedHashMap<Pair<EAPSid, Role>, Integer> stateB = new LinkedHashMap<>();
        stateB.put(new Pair<>(s, B), 0);
        EAPConfig cB = rf.config(p2, tB, sigmaB, stateB);*/
        EACActor cB = RF.config(p2, tB, sigmaB, MF.intt(0));

        System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);

        //---------------

        EALOutType out1u = (EALOutType) parseSessionType(out1us);

        lethA.type(new GammaState(EAVIntType.INT), out1u);

        leth.type(new GammaState(EAVIntType.INT), recXB);

        LinkedHashMap<Pair<EASid, Role>, EALType> env = new LinkedHashMap<>();
        //env.put(new Pair<>(s, A), recXA);
        env.put(new Pair<>(s, A), out1u);
        System.out.println("Typing cA: " + cA + " ,, " + env);
        cA.type(new Gamma(), new Delta(env));

        env = new LinkedHashMap<>();
        env.put(new Pair<>(s, B), recXB);
        System.out.println("Typing cB: " + cB + " ,, " + env);
        cB.type(new Gamma(), new Delta(env));
        //*/

        // ----

        System.out.println("\n---");

        LinkedHashMap<EAPid, EACActor> cs = new LinkedHashMap<>();
        cs.put(p1, cA);
        cs.put(p2, cB);

        env = new LinkedHashMap<>();
        env.put(new Pair<>(s, A), out1u);
        //env.put(new Pair<>(s, B), in1u);  // !!! cf. EAPSystem this.annots.map.get(k2) -- use unfolded as annot -- XXX that only allows that many number of unfoldings during execution
        env.put(new Pair<>(s, B), recXB);
        System.out.println(env);
        EASystem sys = RF.system(LF, new Delta(env), cs);
        System.out.println(sys);
		/*Map<EAPPid, EAPConfig> cfgs = sys.getConfigs();
		//System.out.println("Typing p1/A: " + cfgs.get(p1));
		//cfgs.get(p1).type(new Gamma(), new Delta(env));  // TODO env for p1/A
		System.out.println("Typing p2/B: " + cfgs.get(p2));
		cfgs.get(p2).type(new Gamma(), new Delta(env));*/
        //env.put(new Pair<>(s, A), out1);
        //System.out.println(env);
        ////sys.type(new Gamma(), new Delta(), new Delta(env));
        sys.type();

        typeAndRun(sys, -1);

        /*System.out.println();
        sys = sys.reduce(p1);
        System.out.println(sys);
        env.put(new Pair<>(s, A), out1u);
        env.put(new Pair<>(s, B), recXB);
        System.out.println(env);
        //sys.type(new Gamma(), new Delta(), new Delta(env));
        sys.type(new Gamma(), new Delta());

        sys = sys.reduce(p2);
        System.out.println();
        System.out.println(sys);
        sys.type(new Gamma(), new Delta());

        sys = sys.reduce(p2);
        System.out.println();
        System.out.println(sys);
        sys.type(new Gamma(), new Delta());

        sys = sys.reduce(p2);
        System.out.println();
        System.out.println(sys);
        sys.type(new Gamma(), new Delta());

        sys = sys.reduce(p2);
        System.out.println();
        System.out.println(sys);
        sys.type(new Gamma(), new Delta());

        for (int i = 0; i < 2; i++) {

            sys = sys.reduce(p1);  // p1 send B1!l1
            System.out.println();
            System.out.println(sys);
            sys.type(new Gamma(), new Delta());

            sys = sys.reduce(p1);
            System.out.println();
            System.out.println(sys);
            sys.type(new Gamma(), new Delta());

            sys = sys.reduce(p1);
            System.out.println();
            System.out.println(sys);
            sys.type(new Gamma(), new Delta());

            sys = sys.reduce(p1);
            System.out.println();
            System.out.println(sys);
            sys.type(new Gamma(), new Delta());

            sys = sys.reduce(p1);  // p1 now idle and installed l2 handler
            System.out.println();
            System.out.println(sys);
            sys.type(new Gamma(), new Delta());

            sys = sys.reduce(p2);  // p2 send A!l2
            System.out.println();
            System.out.println(sys);
            sys.type(new Gamma(), new Delta());

            sys = sys.reduce(p2);
            System.out.println();
            System.out.println(sys);
            sys.type(new Gamma(), new Delta());

            sys = sys.reduce(p2);
            System.out.println();
            System.out.println(sys);
            sys.type(new Gamma(), new Delta());

            // !!! l3 stops here -- below, and outer loop, are left over from ex4

            sys = sys.reduce(p2);
            System.out.println();
            System.out.println(sys);
            sys.type(new Gamma(), new Delta());

            sys = sys.reduce(p2);  // p2 now idle with installed l1 handler
            System.out.println();
            System.out.println(sys);
            sys.type(new Gamma(), new Delta());
            //* /
        }*/
    }

    private static void ex4() {
        System.out.println("\n--ex4:");

        String recXAs = "mu X.B?{l2(1).B!{l1(1).X}}";
        String out1us = "B!{l1(1)." + recXAs + "}";
        String in2us = "B?{l2(1)." + out1us + "}";  // unfolding of recXA
        String h2s = "Handler (Int, " + in2us + ")";

        String hts = "{" + in2us + "} 1 -> " + h2s + "{" + recXAs + "}";
        EAMLet lethA = (EAMLet) parseM(
                "let h: " + hts + " <= return"
                        + "  (rec f { " + in2us + "} (w1: 1): " + h2s + " {" + recXAs + "} . return handler B {"
                        + "    {" + out1us + "} d: Int, l2(w2: 1) |->"
                        + "      let y: 1 <= B!l1(()) in let z : " + h2s + " <= [f ()] in suspend z 42"
                        + "  }) "
                        + "in let w3 : 1 <= B!l1(()) in let hh : " + h2s + " <= [h ()] in suspend hh 42");

        String recXBs = "mu X.A?{l1(1).A!{l2(1).X}}";
        String out2mus = "A!{l2(1)." + recXBs + "}";
        String in1us = "A?{l1(1)." + out2mus + "}";
        String h1s = "Handler (Int, " + in1us + ")";

        String htsB = "{" + in1us + "} 1 -> " + h1s + " {" + recXBs + "}";
        EAMLet leth = (EAMLet) parseM(
                "let h: " + htsB + " <= return"
                        + "  (rec f {  " + in1us + "} (w1: 1): " + h1s + "{" + recXBs + "} . return handler A {"
                        + "    {" + out2mus + "} d: Int, l1(w2: 1) |->"
                        + "      let y: 1 <= A!l2(()) in let z : " + h1s + " <= [f ()] in suspend z 42"
                        + "  }) "
                        + "in let hh : " + h1s + " <= [h ()] in suspend hh 42");

        EACActor cA = RF.config(p1, RF.activeThread(lethA, s, A), EMPTY_SIGMA, MF.intt(0));
        EACActor cB = RF.config(p2, RF.activeThread(leth, s, B), EMPTY_SIGMA, MF.intt(0));
        System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);

        //--------------

        EALOutType out1u = (EALOutType) parseSessionType(out1us);
        EALRecType recXB = (EALRecType) parseSessionType(recXBs);

        /*System.out.println(lethA);
        lethA.type(new GammaState(EAVIntType.INT), out1u);
        System.out.println(leth);
        leth.type(new GammaState(EAVIntType.INT), recXB);*/

        typeCheckActor(cA, new Delta(newLinkedMap(sA, out1u)));
        typeCheckActor(cB, new Delta(newLinkedMap(sB, recXB)));

        // ----

        Delta delta = new Delta(newLinkedMap(sA, out1u, sB, recXB));
        EASystem sys = RF.system(LF, delta, newLinkedMap(cA.pid, cA, cB.pid, cB));
        // !!! cf. EAPSystem this.annots.map.get(k2) -- use unfolded as annot -- XXX that only allows that many number of unfoldings during execution

        typeAndRun(sys, 100);
    }

    private static Optional<Exception> ex2(boolean debug) {
        //System.out.println("\n--ex2:");

        EALOutType out1 = (EALOutType) parseSessionType("B!{l1(1).B!{l2(1).end}}");
        EAMLet let = (EAMLet) parseM("let x: 1 <= B!l1(()) in B!l2(())");

        EALInType in1 = (EALInType) parseSessionType("A?{l1(1).A?{l2(1).end}}");
        EATIdle idle = RF.idle();
        EAEHandlers hsB1 = (EAEHandlers) parseV(
                "handler A {"
                        + "  {A?{l2(1).end}} d: Int, l1(x: 1) |->"
                        + "    suspend (handler A { {end} z:Int, l2(x: 1) |-> return z }) 42 "
                        + "}");

        // TODO factor out a Sigma class, e.g., for typing
        EACActor cA = RF.config(p1, RF.activeThread(let, s, A), EMPTY_SIGMA, MF.unit());
        EACActor cB = RF.config(p2, idle, newLinkedMap(sB, hsB1), MF.intt(0));  // B step after active suspend
        /*System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);*/

        // ----

        /*typeCheckActor(cA, new Delta(newLinkedMap(sA, out1)));
        typeCheckActor(cB, new Delta(newLinkedMap(sB, in1)));*/

        // ----

        Delta delta = new Delta(newLinkedMap(sA, out1, sB, in1));
        LinkedHashMap<EAPid, EACActor> cs = newLinkedMap(cA.pid, cA, cB.pid, cB);

        //EASystem sys = RF.system(LF, delta, cs);
        LinkedHashMap<EASid, EAGlobalQueue> queues = EAUtil.mapOf(s, new EAGlobalQueue(s));
        AsyncDelta adelta = new AsyncDelta(EAUtil.copyOf(delta.map), EAUtil.mapOf(s, EAUtil.listOf()));
        EAAsyncSystem sys = RF.asyncSystem(LF, cs, queues, adelta);

        //typeAndRun(sys, -1, true);
        return typeAndRunD(sys, -1, true);
    }
    //*/

    private static boolean runTest(
            String name, Function<Boolean, Optional<Exception>> test, boolean debug) {
        System.out.println("\n-- " + name + ":");
        Optional<Exception> apply = test.apply(debug);
        if (apply.isPresent()) {
            System.err.println(apply.get());
            return false;
        }
        return true;
    }

    //*
    private static Optional<Exception> ex1(boolean debug) {
        //System.out.println("\n---ex1:");

        EALOutType out1 = (EALOutType) parseSessionType("B!{l1(Int).end}");
        //EAMLet sendAB = (EAMLet) parseM("let x: Int <= return 41 + 1 in B!l1(x)");  // TODO refactor binop
        EAMLet sendAB = (EAMLet) parseM("let x: Int <= return 41 in B!l1(x)");

        EALInType in1 = (EALInType) parseSessionType("A?{l1(Int).end}");
        EAEHandlers hsB = (EAEHandlers) parseV("handler A { {end} d: 1, l1(x: Int) |-> return d }");

        EACActor cA = RF.config(p1, RF.activeThread(sendAB, s, A), EMPTY_SIGMA, MF.unit());
        EACActor cB = RF.config(p2, RF.idle(), newLinkedMap(sB, hsB), MF.unit());  // B step after active suspend
        /*System.out.println("cA: " + cA);
        System.out.println("cB: " + cB);*/

        // ---

        /*typeCheckActor(cA, new Delta(newLinkedMap(sA, out1)));
        typeCheckActor(cB, new Delta(newLinkedMap(sB, in1)));*/

        // ---

        LinkedHashMap<EAPid, EACActor> cs = newLinkedMap(cA.pid, cA, cB.pid, cB);
        LinkedHashMap<Pair<EASid, Role>, EALType> env = newLinkedMap(sA, out1, sB, in1);

        Delta delta = new Delta(env);
        //EASystem sys = RF.system(LF, new Delta(env), cs);
        LinkedHashMap<EASid, EAGlobalQueue> queues = EAUtil.mapOf(s, new EAGlobalQueue(s));
        AsyncDelta adelta = new AsyncDelta(EAUtil.copyOf(delta.map), EAUtil.mapOf(s, EAUtil.listOf()));
        EAAsyncSystem sys = RF.asyncSystem(LF, cs, queues, adelta);

        //typeAndRun(sys, -1);
        //typeAndRun(sys, -1, true);
        return typeAndRunD(sys, -1, debug);
    }
    //*/



    // TODO deprecate following -- cf. EAUtil
    static <K, V> LinkedHashMap<K, V> newLinkedMap() {
        return new LinkedHashMap<>();
    }

    static <K, V> LinkedHashMap<K, V> newLinkedMap(List<K> ks, List<V> vs) {
        LinkedHashMap<K, V> res = new LinkedHashMap<>();
        Iterator<V> iv = vs.iterator();
        ks.forEach(x -> res.put(x, iv.next()));
        return res;
    }

    static <K, V> LinkedHashMap<K, V> newLinkedMap(K k, V v) {
        return newLinkedMap(List.of(k), List.of(v));
    }

    static <K, V> LinkedHashMap<K, V> newLinkedMap(K k1, V v1, K k2, V v2) {
        return newLinkedMap(List.of(k1, k2), List.of(v1, v2));
    }

    static <K, V> LinkedHashMap<K, V> newLinkedMap(K k1, V v1, K k2, V v2, K k3, V v3) {
        return newLinkedMap(List.of(k1, k2, k3), List.of(v1, v2, v3));
    }




    /* ... */

    static void typeCheckComp(GammaState gamma, EAComp M, EALType pre) {
        System.out.print("Typing computation: " + M);
        Either<Exception, Pair<Pair<EAVType, EALType>, Tree<String>>> typingA = M.type(gamma, pre);
        if (typingA.isLeft()) {
            throw new RuntimeException(typingA.getLeft());
        }
        Pair<Pair<EAVType, EALType>, Tree<String>> ppA = typingA.getRight();
        System.out.println(" " + pre + " ,, " + ppA.left + "\n" + ppA.right);
    }

    static void typeCheckExpr(GammaState gamma, EAExpr e, EAVType A) {
        System.out.print("Typing expr: " + e);
        Either<Exception, Pair<EAVType, Tree<String>>> t = e.type(gamma);
        if (t.isLeft()) {
            throw new RuntimeException(t.getLeft());
        }
        Pair<EAVType, Tree<String>> p = t.getRight();
        System.out.println(" " + p.left + "\n" + p.right);
        if (!p.left.equals(A)) {
            throw new RuntimeException("Type checking error, expected: " + A);
        }
    }

    static void typeCheckActor(EACActor c, Delta delta) {
        System.out.println("Typing actor: " + c + " ,, " + delta);
        Either<Exception, Tree<String>> t = c.type(new Gamma(), delta);
        if (t.isLeft()) {
            throw new RuntimeException(t.getLeft());
        }
    }

    /* ... */

    static void typeCheckSystem(EASystem sys) {
        typeCheckSystem(sys, false);
    }

    static void typeCheckSystem(EASystem sys, boolean debug) {
        if (debug) {
            System.out.println("Type checking system:");
        }
        Either<Exception, List<Tree<String>>> t = sys.type();
        if (t.isLeft()) {
            throw new RuntimeException(t.getLeft());
        }
        if (debug) {
            System.out.println(t.getRight().stream()
                    .map(x -> x.toString("  ")).collect(Collectors.joining("\n\n")));
        }
    }

    static void typeAndRun(EASystem sys, int steps) {
        typeAndRun(sys, steps, false);
    }

    // steps -1 for unbounded
    static void typeAndRun(EASystem sys, int steps, boolean debug) {
        System.out.println("\nInitial system:\n" + sys);
        typeCheckSystem(sys, debug);

        int rem = steps;
        Map<EAPid, Set<EAPid>> pids = sys.canStep();
        for (; !pids.isEmpty() && rem != 0; rem--) {
            //sys = sys.reduce(pids.keySet().iterator().next());  // FIXME HERE HERE always first act  // keyset is can-step-pids, (currently unused) Set is "partners"
            Pair<EASystem, Tree<String>> reduce = sys.reduce(pids.keySet().iterator().next());
            sys = reduce.left;
            if (debug) {
                System.out.println("\n" + sys);
                System.out.println("\nReduced one step by:");
                System.out.println(reduce.right.toString("  "));
            }
            typeCheckSystem(sys, debug);
            pids = sys.canStep();
        }

        if (!debug) {
            System.out.println();
            System.out.println("Result steps(" + steps + "):");
            System.out.println(sys);
        }

        if (steps == -1) {
            if (sys.actors.values().stream().anyMatch(x -> !x.T.isIdle() || !x.sigma.isEmpty())) {
                throw new RuntimeException("Stuck: " + sys);
            }
        } else if (rem != 0) {
            throw new RuntimeException("Stuck rem=" + rem + ": " + sys);
        }
    }

    /* ... */

    static void typeCheckSystem(EAAsyncSystem sys) {
        typeCheckSystem(sys, false);
    }

    static void typeCheckSystem(EAAsyncSystem sys, boolean debug) {
        typeCheckSystem(sys, debug, "");
    }

    static Optional<Exception> typeCheckSystem(
            EAAsyncSystem sys, boolean debug, String indent) {
        if (debug) {
            System.out.println(indent + "Type checking system:");
        }
        Either<Exception, List<Tree<String>>> t = sys.type();
        if (t.isLeft()) {
            return Optional.of(t.getLeft());
        }
        if (debug) {
            System.out.println(t.getRight().stream()
                    .map(x -> x.toString(indent + "  "))
                    .collect(Collectors.joining("\n\n")));
        }
        return Optional.empty();
    }

    static int state = 0;

    // max -1 for unbounded
    static Optional<Exception> typeAndRunD(EAAsyncSystem sys, int max, boolean debug) {
        state = 0;
        System.out.println("\n(" + state + ") Initial system:\n" + sys);
        return typeAndRunDAux(sys, 0, max, debug, "");
    }

    static Optional<Exception> typeAndRunDAux(
            EAAsyncSystem sys, int depth, int max, boolean debug, String indent) {

        Optional<Exception> opt = typeCheckSystem(sys, debug, indent);
        if (opt.isPresent()) {
            return opt;
        }

        if (max >= 0 && depth >= max) {
            return Optional.empty();
        }

        Map<EAPid, Map<EASid, Either<EAComp, EAMsg>>> pids = sys.getSteppable();
        System.out.println(indent + "Steppable: " + pids);

        if (pids.isEmpty()) {
            if (!debug) {
                System.out.println();
                System.out.println(indent + "Result steps(" + depth + "):");
                System.out.println(sys);
            }
            if (sys.actors.values().stream().anyMatch(x -> !x.T.isIdle() || !x.sigma.isEmpty())) {
                return Optional.of(new Exception(indent + "Stuck: " + sys));
            }
            return Optional.empty();
        }

        int stmp = state;
        int i = 0;
        for (Map.Entry<EAPid, Map<EASid, Either<EAComp, EAMsg>>> pid : pids.entrySet()) {
            EAPid p = pid.getKey();
            Map.Entry<EASid, Either<EAComp, EAMsg>> sid = pid.getValue().entrySet().iterator().next();
            EASid s = sid.getKey();
            Either<EAComp, EAMsg> a = sid.getValue();
            Either<Exception, Triple<EAAsyncSystem, Tree<String>, Tree<String>>> step;
            System.out.print("\n" + indent + "(" + stmp + "-" + i + ") ");
            if (a.isLeft()) {
                EAComp e = a.getLeft();
                System.out.println("Stepping " + s + "@" + p + ": " + e);
                step = sys.step(p, s, e);
            } else {
                EAMsg m = a.getRight();
                System.out.println("Reacting " + s + "@" + p + ": " + m);
                step = sys.react(p, s, m);
            }
            if (step.isLeft()) {
                throw new RuntimeException(step.getLeft());
            }
            Triple<EAAsyncSystem, Tree<String>, Tree<String>> get = step.getRight();
            state++;
            i++;

            String indent1 = incIndent(indent);
            EAAsyncSystem sys1 = get.left;
            if (debug) {
                System.out.println(indent1 + "(" + state + ")\n" + sys1.toString(indent1));
                System.out.println(indent1 + "Reduced one step by:");
                System.out.println(get.mid.toString(indent1));
                if (get.right != null) {  // XXX TODO
                    System.out.println(indent1 + "Delta stepped by:");
                    System.out.println(get.right.toString(indent1));
                }
            }
            Optional<Exception> run = typeAndRunDAux(sys1, depth + 1, max, debug, incIndent(indent));
            if (run.isPresent()) {
                return run;
            }
        }
        return Optional.empty();
    }

    static String incIndent(String m) {
        return m.isEmpty() ? ".   " : m + ".   ";
    }

    static void typeAndRun(EAAsyncSystem sys, int steps) {
        typeAndRun(sys, steps, false);
    }

    // steps -1 for unbounded
    static void typeAndRun(EAAsyncSystem sys, int steps, boolean debug) {
        System.out.println("\nInitial system:\n" + sys);
        typeCheckSystem(sys, debug);

        int rem = steps;
        Map<EAPid, Map<EASid, Either<EAComp, EAMsg>>> pids = sys.getSteppable();
        for (; !pids.isEmpty() && rem != 0; pids = sys.getSteppable()) {

            Map.Entry<EAPid, Map<EASid, Either<EAComp, EAMsg>>> pid =
                    pids.entrySet().iterator().next();  // FIXME HERE HERE first pid -> all
            EAPid p = pid.getKey();
            Map.Entry<EASid, Either<EAComp, EAMsg>> sid = pid.getValue().entrySet().iterator().next();
            EASid s = sid.getKey();
            Either<EAComp, EAMsg> a = sid.getValue();
            Either<Exception, Triple<EAAsyncSystem, Tree<String>, Tree<String>>> step;
            if (a.isLeft()) {
                EAComp e = a.getLeft();
                System.out.println("\nStepping " + s + "@" + p + ": " + e);
                step = sys.step(p, s, e);
            } else {
                EAMsg m = a.getRight();
                System.out.println("\nReacting " + s + "@" + p + ": " + m);
                step = sys.react(p, s, m);
            }
            if (step.isLeft()) {
                throw new RuntimeException(step.getLeft());
            }
            Triple<EAAsyncSystem, Tree<String>, Tree<String>> get = step.getRight();

            sys = get.left;
            if (debug) {
                System.out.println(sys);
                System.out.println("  Reduced one step by:");
                System.out.println(get.mid.toString("    "));
                if (get.right != null) {  // XXX TODO
                    System.out.println("  Delta stepped by:");
                    System.out.println(get.right.toString("    "));
                }
            }
            typeCheckSystem(sys, debug);
            rem--;
        }

        if (!debug) {
            System.out.println();
            System.out.println("Result steps(" + steps + "):");
            System.out.println(sys);
        }

        if (steps == -1) {
            if (sys.actors.values().stream().anyMatch(x -> !x.T.isIdle() || !x.sigma.isEmpty())) {
                throw new RuntimeException("Stuck: " + sys);
            }
        } else if (rem != 0) {
            throw new RuntimeException("Stuck rem=" + rem + ": " + sys);
        }
    }


    /* parsing */

    static void testParser() {
        EATermFactory pf = EATermFactory.factory;
        EARuntimeFactory rf = EARuntimeFactory.factory;
        EATypeFactory tf = EATypeFactory.factory;

        //String input = "(A ! a((())))";
        String input = "let x: 1 <= A ! a((())) in suspend "
                + "(handler A {b(x: 1): A?{b(1).C!{c(1).end}} -> return (), c(y: 1): end -> return ()})";
        EATerm res = parseM(input);
        System.out.println("bbb: " + res);
    }

    static EAComp parseM(String input) {
        Lexer lex = new EACalculusLexer(new ANTLRStringStream(input));
        EACalculusParser par = new EACalculusParser(new CommonTokenStream(lex));
        try {
            //par.setTreeAdaptor(new EATreeAdaptor());  // XXX this requires nodes to be CommonTree to add children -- adaptor only constructs each node individualy without children
            CommonTree tree = (CommonTree) par.start().getTree();
            //System.out.println("aaa: " + tree.getClass() + "\n" + tree.getText() + " ,, " + tree.getChild(0) + " ,, " + tree.getChild(1));

            EAComp res = new EAFuncNamesFixer().parse(new EAASTBuilder().visitM((CommonTree) tree.getChild(0)));
            return res;

            //tree.token;
        } catch (RecognitionException x) {
            throw new RuntimeException(x);
        }
    }

    static EAVType parseA(String input) {
        Lexer lex = new EACalculusLexer(new ANTLRStringStream(input));
        EACalculusParser par = new EACalculusParser(new CommonTokenStream(lex));
        try {
            CommonTree tree = (CommonTree) par.type().getTree();
            return new EAASTBuilder().visitA(tree);
        } catch (RecognitionException x) {
            throw new RuntimeException(x);
        }
    }

    static EAExpr parseV(String input) {
        Lexer lex = new EACalculusLexer(new ANTLRStringStream(input));
        EACalculusParser par = new EACalculusParser(new CommonTokenStream(lex));
        try {
            CommonTree tree = (CommonTree) par.nV().getTree();
            return new EAFuncNamesFixer().parse(new EAASTBuilder().visitV(tree));
        } catch (RecognitionException x) {
            throw new RuntimeException(x);
        }
    }

    static EALType parseSessionType(String input) {
        Lexer lex = new EACalculusLexer(new ANTLRStringStream(input));
        EACalculusParser par = new EACalculusParser(new CommonTokenStream(lex));
        try {
            CommonTree tree = (CommonTree) par.session_type().getTree();
            return new EAASTBuilder().visitSessionType(tree);
        } catch (RecognitionException x) {
            throw new RuntimeException(x);
        }
    }


	/*static class EATreeAdaptor extends CommonTreeAdaptor {
		static EAPFactory pf = EAPFactory.factory;
		static EAPRuntimeFactory rf = EAPRuntimeFactory.factory;
		static EATypeFactory tf = EATypeFactory.factory;

		public EATreeAdaptor() {
		}

		// Generated parser seems to use nil to create "blank" nodes and then "fill them in"
		//@Override public Object nil() {return new ScribNil();}

		// Create a Tree (ScribNode) from a Token
		// N.B. not using AstFactory, construction here is pre adding children (and also here directly record parsed Token, not recreate)
		@Override public EAPVal create(Token t) {
			switch (t.getText()) {
				case "RETURN": {
						pf.returnn()
				}
			}
		}
	}*/
}
