package org.scribble.ext.ea.cli;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.Lexer;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;
import org.scribble.cli.CommandLine;
import org.scribble.cli.CommandLineException;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.local.LTypeFactory;
import org.scribble.core.type.session.local.LTypeFactoryImpl;
import org.scribble.ext.ea.core.runtime.*;
import org.scribble.ext.ea.core.runtime.config.EACActor;
import org.scribble.ext.ea.core.term.EAName;
import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.term.EATermFactory;
import org.scribble.ext.ea.core.term.comp.EAComp;
import org.scribble.ext.ea.core.term.comp.EAMLet;
import org.scribble.ext.ea.core.term.expr.EAEFuncName;
import org.scribble.ext.ea.core.term.expr.EAEHandlers;
import org.scribble.ext.ea.core.term.expr.EAEVar;
import org.scribble.ext.ea.core.term.expr.EAExpr;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.*;
import org.scribble.ext.ea.core.type.value.*;
import org.scribble.ext.ea.parser.antlr.EACalculusLexer;
import org.scribble.ext.ea.parser.antlr.EACalculusParser;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.AntlrSourceException;
import org.scribble.util.Pair;

import java.util.*;


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


// Includes assrt-core functionality (all extra args are currently for assrt-core)
public class EACommandLine extends CommandLine {

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

        //CommandLine.main(args);  // !!! base CommandLine fully bypassed -- No main module used
        eamain();
        //testParser();
    }

    private static void tests() {

        //System.out.println(parseV("2 + 3"));

        ex1();
        ex2();

        ex4();
        /*System.out.println("\n---\nex5");
        ex5(lf, pf, rf, tf);

        System.out.println("\n---\nex6");
        ex6(lf, pf, rf, tf);
        System.out.println("\n---\nex7");
        ex7(lf, pf, rf, tf);
        System.out.println("\n---\nex8");
        ex8(lf, pf, rf, tf);

        System.out.println("\n---\nex10");
        ex10(lf, pf, rf, tf);*/
    }

    private static void negtests() {
        LTypeFactoryImpl lf = new LTypeFactoryImpl();

        EATermFactory pf = EATermFactory.factory;
        EARuntimeFactory rf = EARuntimeFactory.factory;
        EATypeFactory tf = EATypeFactory.factory;

        //ex9(lf, pf, rf, tf);
    }

    private static void eamain() {
        LTypeFactoryImpl lf = new LTypeFactoryImpl();

        EATermFactory pf = EATermFactory.factory;
        EARuntimeFactory rf = EARuntimeFactory.factory;
        EATypeFactory tf = EATypeFactory.factory;

        //System.out.println(parseV("2 + 3"));

        tests();

        /* HERE HERE
        // merge rhu1-refactorinterfaces -- i.e., latest scrib-core

        - ...coherence (etc.) -- cf. EAPSystem.annots
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

        //new EACommandLine(args).run();
    }

    static void ex10(LTypeFactory lf, EATermFactory pf, EARuntimeFactory rf, EATypeFactory tf) {
        Role A = new Role("A");
        Role B = new Role("B");
        EASid s = rf.sid("s");
        EAPid p1 = rf.pid("p1");
        EAPid p2 = rf.pid("p2");

        //---------------

        EAComp lethA = parseM("return 42");

        //---------------
        // config < A, idle, c[A] |-> let h = ... in ... >
        EATActive tA = rf.activeThread(lethA, s, A);
        LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigmaA = new LinkedHashMap<>();
        EACActor cA = rf.config(p1, tA, sigmaA, pf.factory.intt(1));

        System.out.println();
        EALType out1 = parseSessionType("end");
        LinkedHashMap<Pair<EASid, Role>, EALType> env = new LinkedHashMap<>();
        env.put(new Pair<>(s, A), out1);
        System.out.println("Typing cA: " + cA + " ,, " + env);
        cA.type(new Gamma(), new Delta(env));

        //---------------

        EAComp lethB = parseM("return 43");

        //--------------
        // config < B, idle, c[B] |-> let h = ... in ... } >
        EATActive tB = rf.activeThread(lethB, s, B);
        LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigmaB = new LinkedHashMap<>();
        EACActor cB = rf.config(p2, tB, sigmaB, pf.factory.intt(2));

        System.out.println();
        env = new LinkedHashMap<>();
        EALType in1 = parseSessionType("end");
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
        sys.type(new Gamma(), new Delta());

        typeAndRun(sys, -1);
        //
    }

    // !!! Not WT -- testing (incompatible) state typing
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
        sys.type(new Gamma(), new Delta());

        typeAndRun(sys, -1);
        //
    }

    static void ex8(LTypeFactory lf, EATermFactory pf, EARuntimeFactory rf, EATypeFactory tf) {
        Role A = new Role("A");
        Role B = new Role("B");
        EASid s = rf.sid("s");
        EAPid p1 = rf.pid("p1");
        EAPid p2 = rf.pid("p2");

        String out1s = "B!{l1(1).X}";
        String in2s = "B?{l2(1)." + out1s + ", l3(1).end }";
        String recXAs = "mu X." + in2s;
        String out1us = "B!{l1(1)." + recXAs + "}";
        String in2us = "B?{l2(1)." + out1us + ", l3(1).end}";  // unfolding of recXA

        // ----

        String h2s = "Handler (Int, " + in2us + ")";
        String hts = "{" + recXAs + "} 1 -> " + h2s + "{" + recXAs + "}";
        EAMLet lethA = (EAMLet) parseM(
                "let h: " + hts + " <= return (rec f { " + recXAs + "} (w1: 1 ):" + h2s
                        + "{" + recXAs + "} . return handler B { {" + out1us + "}"
                        + " z2: Int, l2(w2: 1)  |-> let y: 1 <= B!l1(()) in"
                        + " let z : " + h2s + " <= [f ()] in suspend z 42,"
                        + "  {end} z3: Int, l3(w3: 1) |-> return z3 })"
                        + "in let w3 : 1 <= B!l1(()) in let hh : " + h2s + " <= [h ()] in suspend hh 0");

        System.out.println(lethA);
        EALOutType out1u = (EALOutType) parseSessionType(out1us);
        lethA.type(new GammaState(EAVIntType.INT), out1u);

        //---------------
        // config < A, idle, c[A] |-> let h = ... in ... >
        System.out.println();

        EATActive tA = rf.activeThread(lethA, s, A);
        LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigmaA = new LinkedHashMap<>();
        EACActor cA = rf.config(p1, tA, sigmaA, pf.factory.intt(0));
        //EAPConfig<?> cA = rf.config(p1, tA, sigmaA, pf.factory.bool(true));

        LinkedHashMap<Pair<EASid, Role>, EALType> env = new LinkedHashMap<>();
        env.put(new Pair<>(s, A), out1u);
        System.out.println("Typing cA: " + cA + " ,, " + env);
        cA.type(new Gamma(), new Delta(env));

        // ----
        System.out.println();

        String out2s = "A!{l2(1).X, l3(1).end}";
        String in1s = "A?{l1(1)." + out2s + "}";
        String recXBs = "mu X." + in1s;
        String out2mus = "A!{l2(1)." + recXBs + ", l3(1).end}";
        String in1us = "A?{l1(1)." + out2mus + "}";

        // ---

        String h1s = "Handler (Int, " + in1us + ")";
        String htsB = "{" + in1us + "} 1 ->" + h1s + "{" + recXBs + "}";
        EAMLet leth = (EAMLet) parseM(
                "let h: " + htsB + " <= return (rec f{  " + in1us + "} (w1: 1):" + h1s + "{" + recXBs + "} ."
                        + "return handler A { {" + out2mus + "} d: Int, l1(w2: 1) "

                        /*//+ " |-> let y: 1 <= A!l2(()) in let z : " + h1s + " <= [f ()] in suspend z 42 })"  // run forever -- old
                        + " |-> let y: 1 <= A!l3(()) in return () })"*/  // quit straight away

                        //+ " |-> let tmp: Bool <= return d < 0 in "  // quit straight away -- 0
                        + " |-> let tmp: Bool <= return d < 42 in "  // quit after one -- 42
                        //+ " |-> let tmp: Bool <= return d < 43 in "  // run forever -- change run(-1) below

                        + "if tmp then let y: 1 <= A!l2(()) in let z : " + h1s + " <= [f ()] in suspend z 42"
                        + "else let y: 1 <= A!l3(()) in return d"

                        + " })"
                        + "in let hh : " + h1s + " <= [h ()] in suspend hh 0");

        System.out.println(leth);
        EALRecType recXB = (EALRecType) parseSessionType(recXBs);
        leth.type(new GammaState(EAVIntType.INT), recXB);

        //--------------
        // config < B, idle, c[B] |-> let h = ... in ... } >
        System.out.println();

        EATActive tB = rf.activeThread(leth, s, B);
        LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigmaB = new LinkedHashMap<>();
        EACActor cB = rf.config(p2, tB, sigmaB, pf.factory.intt(0));

        env = new LinkedHashMap<>();
        env.put(new Pair<>(s, B), recXB);
        System.out.println("Typing cB: " + cB + " ,, " + env);
        cB.type(new Gamma(), new Delta(env));
        //*/

        // ----

        System.out.println("\n---");
        System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);

        LinkedHashMap<EAPid, EACActor> cs = new LinkedHashMap<>();
        cs.put(cA.pid, cA);
        cs.put(cB.pid, cB);

        env = new LinkedHashMap<>();
        env.put(new Pair<>(s, A), out1u);
        env.put(new Pair<>(s, B), recXB);
        System.out.println(env);
        EASystem sys = rf.system(lf, new Delta(env), cs);
        System.out.println(sys);
        sys.type(new Gamma(), new Delta());

        //run(sys, 100);  // run forever
        typeAndRun(sys, -1);  // quit straight away or after one
    }

    static void ex7(LTypeFactory lf, EATermFactory pf, EARuntimeFactory rf, EATypeFactory tf) {
        Role A = new Role("A");
        Role B = new Role("B");
        EASid s = rf.sid("s");
        EAPid p1 = rf.pid("p1");
        EAPid p2 = rf.pid("p2");

        String out1s = "B!{l1(1).X, l4(1).end}";
        String in2s = "B?{l2(1)." + out1s + ", l3(1).end }";
        String recXAs = "mu X." + in2s;
        String out1us = "B!{l1(1)." + recXAs + ", l4(1).end}";
        String in2us = "B?{l2(1)." + out1us + ", l3(1).end}";  // unfolding of recXA

        // ----

        String h2s = "Handler (Int, " + in2us + ")";
        String hts = "{" + recXAs + "} 1-> " + h2s + "{" + recXAs + "}";
        EAMLet lethA = (EAMLet) parseM(
                "let h: " + hts + " <= return (rec f { " + recXAs + "} (w1: 1 ):" + h2s + "{" + recXAs
                        + "} . return handler B { {" + out1us + "} z2: Int, l2(w2: 1) "
                        + " |-> let y: 1 <= B!l4(()) in return z2"
                        + ",  {end} z3: Int, l3(w3: 1) |-> return z3 })"
                        + "in let w3 : 1 <= B!l1(()) in let hh : " + h2s + " <= [h ()] in suspend hh 0");

        System.out.println(lethA);
        EALOutType out1u = (EALOutType) parseSessionType(out1us);
        lethA.type(new GammaState(EAVIntType.INT), out1u);

        //---------------
        // config < A, idle, c[A] |-> let h = ... in ... >
        System.out.println();

        EATActive tA = rf.activeThread(lethA, s, A);
        LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigmaA = new LinkedHashMap<>();
        EACActor cA = rf.config(p1, tA, sigmaA, pf.factory.intt(0));

        LinkedHashMap<Pair<EASid, Role>, EALType> env = new LinkedHashMap<>();
        env.put(new Pair<>(s, A), out1u);
        System.out.println("Typing cA: " + cA + " ,, " + env);
        cA.type(new Gamma(), new Delta(env));

        // ----
        System.out.println();

        String out2s = "A!{l2(1).X, l3(1).end}";
        String in1s = "A?{l1(1)." + out2s + ", l4(1).end}";
        String recXBs = "mu X." + in1s;
        String out2mus = "A!{l2(1)." + recXBs + ", l3(1).end}";
        String in1us = "A?{l1(1)." + out2mus + ", l4(1).end}";

        // ---

        String h1s = "Handler (Int, " + in1us + ")";
        String htsB = "{" + in1us + "} 1 ->" + h1s + "{" + recXBs + "}";
        EAMLet leth = (EAMLet) parseM(
                "let h: " + htsB + " <= return (rec f{  " + in1us + "} (w1: 1):" + h1s + "{" + recXBs + "} ."
                        + "return handler A { {" + out2mus + "} d: Int, l1(w2: 1) "
                        + " |-> let y: 1 <= A!l2(()) in let z : " + h1s + " <= [f ()] in suspend z 42 "
                        + ", {end} d: Int, l4(w4: 1) |-> return d"
                        + "})"
                        + "in let hh : " + h1s + " <= [h ()] in suspend hh 0");

        System.out.println(leth);
        EALRecType recXB = (EALRecType) parseSessionType(recXBs);
        leth.type(new GammaState(EAVIntType.INT), recXB);

        //--------------
        // config < B, idle, c[B] |-> let h = ... in ... } >
        System.out.println();

        EATActive tB = rf.activeThread(leth, s, B);
        LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigmaB = new LinkedHashMap<>();
        EACActor cB = rf.config(p2, tB, sigmaB, pf.factory.intt(0));

        env = new LinkedHashMap<>();
        env.put(new Pair<>(s, B), recXB);
        System.out.println("Typing cB: " + cB + " ,, " + env);
        cB.type(new Gamma(), new Delta(env));
        //*/

        // ----

        System.out.println("\n---");
        System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);

        LinkedHashMap<EAPid, EACActor> cs = new LinkedHashMap<>();
        cs.put(cA.pid, cA);
        cs.put(cB.pid, cB);

        env = new LinkedHashMap<>();
        env.put(new Pair<>(s, A), out1u);
        env.put(new Pair<>(s, B), recXB);
        System.out.println(env);
        EASystem sys = rf.system(lf, new Delta(env), cs);
        System.out.println(sys);
        sys.type(new Gamma(), new Delta());

        typeAndRun(sys, -1);
    }

    private static void ex6(
            LTypeFactory lf, EATermFactory pf, EARuntimeFactory rf, EATypeFactory tf) {

        Role A = new Role("A");
        Role B = new Role("B");
        EASid s = rf.sid("s");
        EAPid p1 = rf.pid("p1");
        EAPid p2 = rf.pid("p2");

        EALOutType out1 = (EALOutType) parseSessionType("B!{l1(Bool).end}");
        EALInType in1 = (EALInType) parseSessionType("A?{l1(Bool).end}");
        EAComp sendAB = parseM("let x: Bool <= return 3 < 2 in if x then B!l1(x) else B!l1(x)");
        EATActive tA = rf.activeThread(sendAB, s, A);
        LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigmaA = new LinkedHashMap<>();
        /*LinkedHashMap<Pair<EAPSid, Role>, Integer> stateA = new LinkedHashMap<>();
        stateA.put(new Pair<>(s, A), 0);
        EAPConfig cA = rf.config(p1, tA, sigmaA, stateA);*/
        EACActor cA = rf.config(p1, tA, sigmaA, pf.factory.unit());

        EAEVar x = pf.var("x");
        EAEHandlers hsB = (EAEHandlers) parseV("handler A { {end} z1: 1, l1(x: Bool)  |-> return () }");
        EATIdle idle = rf.idle();
        LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigmaB = new LinkedHashMap<>();
        sigmaB.put(new Pair<>(s, B), hsB);
        /*LinkedHashMap<Pair<EAPSid, Role>, Integer> stateB = new LinkedHashMap<>();
        stateB.put(new Pair<>(s, B), 42);
        EAPConfig cB = rf.config(p2, idle, sigmaB, stateB);*/
        EACActor cB = rf.config(p2, idle, sigmaB, pf.factory.unit());

        System.out.println(cA);
        System.out.println(cB);

        System.out.println("Typing eA: " + out1 + " ,, " + sendAB.type(new GammaState(EAVIntType.INT), out1));

        LinkedHashMap<Pair<EASid, Role>, EALType> env = new LinkedHashMap<>();
        env.put(new Pair<>(s, A), out1);
        System.out.println("Typing cA: " + cA + " ,, " + env);
        cA.type(new Gamma(), new Delta(env));

        LinkedHashMap<EAName, EAVType> map = new LinkedHashMap<>();
        map.put(x, tf.val.unit());  // XXX FIXME
        GammaState gamma = new GammaState(map, new LinkedHashMap<>(), EAVUnitType.UNIT);
        System.out.println("Typing hB: " + hsB.type(gamma));

        env = new LinkedHashMap<>();
        env.put(new Pair<>(s, B), in1);
        System.out.println("Typing cB: " + cB + " ,, " + env);
        cB.type(new Gamma(), new Delta(env));

        LinkedHashMap<EAPid, EACActor> cs = new LinkedHashMap<>();
        cs.put(p1, cA);
        cs.put(p2, cB);
        //EAPSystem sys = rf.system(cs);
        env.put(new Pair<>(s, A), out1);
        EASystem sys = rf.system(lf, new Delta(env), cs);
        System.out.println(sys);
        sys.type(new Gamma(), new Delta());

        typeAndRun(sys, -1);
    }

    private static void ex5(
            LTypeFactory lf, EATermFactory pf, EARuntimeFactory rf, EATypeFactory tf) {
        Role A = new Role("A");
        Role B = new Role("B");
        Op l1 = new Op("l1");
        Op l2 = new Op("l2");
        Op l3 = new Op("l3");
        //EAPUnit unit = pf.unit();
        EASid s = rf.sid("s");
        EAPid p1 = rf.pid("p1");
        EAPid p2 = rf.pid("p2");
        EAEVar h = pf.var("h");
        EAEVar hh = pf.var("hh");
        EAEVar x = pf.var("x");
        EAEVar y = pf.var("y");
        EAEVar z = pf.var("z");
        EAEVar zz = pf.var("zz");
        EAEVar w1 = pf.var("w1");
        EAEVar w2 = pf.var("w2");
        EAEFuncName f = new EAEFuncName("f");
        RecVar X = new RecVar("X");

        // ----

        // mu X . p&{ l2(unit) . p+{ l1(unit) . X) }, l3(unit) . end }
        LinkedHashMap<Op, Pair<EAVType, EALType>> cases = new LinkedHashMap<>();
        /*cases.put(l1, new Pair<>(tf.val.unit(), tf.local.recvar(X)));
        EALOutType out1 = tf.local.out(B, cases);
        cases = new LinkedHashMap<>();
        cases.put(l2, new Pair<>(tf.val.unit(), out1));
        cases.put(l3, new Pair<>(tf.val.unit(), tf.local.end()));
        EALInType in2 = tf.local.in(B, cases);
        EALRecType recXA = tf.local.rec(X, in2);*/
        String out1s = "B!{l1(1).X}";
        EALOutType out1 = (EALOutType) parseSessionType(out1s);
        String in2s = "B?{l2(1)." + out1s + ", l3(1).end}";
        EALInType in2 = (EALInType) parseSessionType(in2s);
        String recXAs = "mu X . " + in2s;
        EALRecType recXA = (EALRecType) parseSessionType(recXAs);

        // p+{ l1(unit) . [mu X . p&{ l2(unit) . p+{ l1(unit) . X) }, l3(unit) . end }] }
        /*cases = new LinkedHashMap<>();
        cases.put(l1, new Pair<>(tf.val.unit(), recXA));
        EALOutType out1u = tf.local.out(B, cases);*/
        String out1us = "B!{l1(1)." + recXA + "}";
        EALOutType out1u = (EALOutType) parseSessionType(out1us);

        /*cases = new LinkedHashMap<>();
        cases.put(l2, new Pair<>(tf.val.unit(), out1u));
        cases.put(l3, new Pair<>(tf.val.unit(), tf.local.end()));
        EALInType in2u = tf.local.in(B, cases);
        EAHandlersType h2 = tf.val.handlers(in2u);*/
        String in2us = "B?{l2(1)." + out1us + ", l3(1).end}";
        EALInType in2u = (EALInType) parseSessionType(in2us);
        String h2s = "Handler(Int, " + in2us + ")";
        EAVHandlersType h2 = (EAVHandlersType) parseA(h2s);

        // ----
        // let h = return rec f(_). handler B { l2(_) |-> let y = B!l1() in let z = f() in suspend z
        // 										l3(_) |-> return () }
        // in [ let _ = B!l1() in let hh = h() in suspend hh ]

        //let z = f() in suspend z
        /*EAPSuspend suszA = pf.suspend(z);
        EAPApp appfA = pf.app(f, pf.unit());
        EAPLet letzA = pf.let(z, h2, appfA, suszA);
        System.out.println(letzA);
        //letz.type(new Gamma(), in1);  // Gamma need f

        // return rec f(_). return handler B { l2(_) |-> let y = B!l1() in ... }
        EAPSend sendAB1 = pf.send(B, l1, pf.unit());
        EAPLet letyA = pf.let(y, tf.val.unit(), sendAB1, letzA);
        LinkedHashMap<Op, EAPHandler> HsA = new LinkedHashMap<>();
        EAPHandler hA1 = pf.handler(l2, w2, tf.val.unit(), letyA, out1u);
        HsA.put(l2, hA1);
        EAPReturn retend = pf.returnn(pf.unit());
        EAPHandler hA2 = pf.handler(l3, w2, tf.val.unit(), retend, tf.local.end());
        HsA.put(l3, hA2);
        EAPHandlers hsA2 = pf.handlers(B, HsA);
        EAPReturn rethA2 = pf.returnn(hsA2);
        EAPRec recfA = pf.rec(f, w1, tf.val.unit(), rethA2, in2u, recXA, h2);
        EAPReturn retfA = pf.returnn(recfA);
        System.out.println(retfA);
        retfA.type(new Gamma(), in2);

        // let _ = B!l1() in let hh = h() in suspend hh
        EAPSuspend sushhA = pf.suspend(hh);
        EAPApp apphA = pf.app(h, pf.unit());
        EAPLet lethhA = pf.let(hh, h2, apphA, sushhA);
        EAPLet wA = pf.let(w1, tf.val.unit(), sendAB1, lethhA);
        System.out.println(wA);

        // let h = return rec f(_). ... in [ let _ ... ]
        EAFuncType ftA = tf.val.func(tf.val.unit(), in2u, recXA, h2);
        EAPLet lethA = pf.let(h, ftA, retfA, wA);*/
        String ftAs = "{" + in2us + "} 1 -> " + h2s + "{" + recXAs + "}";
        EAVFuncType ftA = (EAVFuncType) parseA(ftAs);
        EAMLet lethA = (EAMLet) parseM(
                "let h : " + ftAs + " <= return rec f {" + in2us + "} (w1 :1) : " + h2s + " {" + recXAs
                        + "} . return handler B { {" + out1us + "} z2: Int, l2(w2: 1) |-> let y :1 <= B!l1(())"
                        + "in let z : " + h2s + " <= [f ()] in suspend z 42 ,"
                        + "{end} z3: Int, l3(w2: 1) |-> return z3 } "
                        + "in let w1 :1 <= B!l1(()) in let hh: " + h2s + " <= [h ()] in suspend hh 42");

        System.out.println(lethA);
        lethA.type(new GammaState(EAVIntType.INT), out1u);

        //---------------
        // config < A, idle, c[A] |-> let h = ... in ... >
        System.out.println();

        EATActive tA = rf.activeThread(lethA, s, A);
        LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigmaA = new LinkedHashMap<>();
        /*LinkedHashMap<Pair<EAPSid, Role>, Integer> stateA = new LinkedHashMap<>();
        stateA.put(new Pair<>(s, A), 0);
        EAPConfig cA = rf.config(p1, tA, sigmaA, stateA);*/
        EACActor cA = rf.config(p1, tA, sigmaA, pf.factory.intt(0));

        LinkedHashMap<Pair<EASid, Role>, EALType> env = new LinkedHashMap<>();
        //env.put(new Pair<>(s, A), recXA);
        env.put(new Pair<>(s, A), out1u);
        System.out.println("Typing cA: " + cA + " ,, " + env);
        cA.type(new Gamma(), new Delta(env));

        // ----
        System.out.println();

        // !!! no branch/select subtyping
        // mu X . p&{ l1(unit) . p+{ l2(unit) . X, l3(unit).end ) } }
        /*cases = new LinkedHashMap<>();
        cases.put(l2, new Pair<>(tf.val.unit(), tf.local.recvar(X)));
        cases.put(l3, new Pair<>(tf.val.unit(), tf.local.end()));
        EALOutType out2 = tf.local.out(A, cases);
        cases = new LinkedHashMap<>();
        cases.put(l1, new Pair<>(tf.val.unit(), out2));
        EALInType in1 = tf.local.in(A, cases);
        EALRecType recXB = tf.local.rec(X, in1);*/
        String out2s = "A!{ l2(1) . X, l3(1).end }";
        //EALOutType out2 = (EALOutType) parseSessionType(out2s);
        String in1s = "A?{ l1(1) . " + out2s + " }";
        //EALInType in1 = (EALInType) parseSessionType(in1s);
        String recXBs = "mu X . " + in1s;
        EALRecType recXB = (EALRecType) parseSessionType(recXBs);

        /*cases = new LinkedHashMap<>();
        cases.put(l2, new Pair<>(tf.val.unit(), recXB));
        cases.put(l3, new Pair<>(tf.val.unit(), tf.local.end()));
        EALOutType out2mu = tf.local.out(A, cases);*/
        String out2mus = "A!{l2(1) . " + recXBs + ", l3(1) . end }";
        EALOutType out2mu = (EALOutType) parseSessionType(out2mus);

        // p&{ l1(unit) . p+{ l2(unit) . [mu X . p&{ l1(unit) . p+{ l2(unit) . X, l3(unit).end ) } }], l3(unit).end } }
        /*cases = new LinkedHashMap<>();
        cases.put(l2, new Pair<>(tf.val.unit(), recXB));
        cases.put(l3, new Pair<>(tf.val.unit(), tf.local.end()));
        EALOutType out2u = tf.local.out(A, cases);
        cases = new LinkedHashMap<>();
        cases.put(l1, new Pair<>(tf.val.unit(), out2u));
        EALInType in1u = tf.local.in(A, cases);*/
        String out2us = out2mus;
        //EALOutType out2u = (EALOutType) parseSessionType(out2us);
        String in1us = "A?{l1(1) . " + out2us + "}";
        //EALInType in1u = (EALInType) parseSessionType(in1us);

        //EAHandlersType h1 = tf.val.handlers(in1u);
        String h1s = "Handler(Int, " + in1us + ")";
        EAVHandlersType h1 = (EAVHandlersType) parseA(h1s);
        ////EAHandlersType h1fold = tf.val.handlers(recXB);

        // ---
        // let h = return rec f(_). handler A { l1(_) |-> let y = A!l2() in let z = f() in suspend z }
        // in [ let hh = h() in suspend hh ]

        /*//let z = f() in suspend z
        EAPSuspend susz = pf.suspend(z);
        EAPApp appf = pf.app(f, pf.unit());
        EAPLet letz = pf.let(z, h1, appf, susz);
        System.out.println(letz);
        //letz.type(new Gamma(), in1);  // Gamma need f

        // return rec f(_). return handler A { l1(_) |-> let y = A!l2() in ... }
        EAPSend sendBA2 = pf.send(A, l2, pf.unit());
        EAPLet lety = pf.let(y, tf.val.unit(), sendBA2, letz);
        LinkedHashMap<Op, EAPHandler> HsB = new LinkedHashMap<>();

        // !!! TODO if-else
        // return rec f(_). return handler A { l1(_) |-> let y = A!l3() in return () }
        EAPSend sendBA3 = pf.send(A, l3, pf.unit());
        EAPReturn retendB = pf.returnn(pf.unit());
        EAPLet lety3 = pf.let(y, tf.val.unit(), sendBA3, retendB);

        // !!!
        //EAPHandler hB1 = pf.handler(l1, w2, tf.val.unit(), lety, out2mu);
        EAPHandler hB1 = pf.handler(l1, w2, tf.val.unit(), lety3, out2mu);

        HsB.put(l1, hB1);
        EAPHandlers hsB1 = pf.handlers(A, HsB);
        EAPReturn rethB1 = pf.returnn(hsB1);
        EAPRec recfB = pf.rec(f, w1, tf.val.unit(), rethB1, in1u, recXB, h1);
        EAPReturn retfB = pf.returnn(recfB);
        System.out.println(retfB);
        retfB.type(new Gamma(), in1);

        //let hh = h() in suspend hh
        EAPSuspend sushh = pf.suspend(hh);
        EAPApp apph = pf.app(h, pf.unit());
        EAPLet lethh = pf.let(hh, h1, apph, sushh);
        System.out.println(lethh);*/

        // let h = return rec f(_). ... in let hh ...
        //EAFuncType ft = tf.val.func(tf.val.unit(), in1u, recXB, h1);
        String fts = "{" + in1us + "} 1 -> " + h1s + "{" + recXBs + "}";
        //EAFuncType ft = (EAFuncType) parseA(fts);

        //EAPLet leth = pf.let(h, ft, retfB, lethh);
        EAMLet leth = (EAMLet) parseM(
                //"let h : {A?{l1(1).A!{l2(1).mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}, l3(1).end}}}1 -> Handler(A?{l1(1).A!{l2(1).mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}, l3(1).end}}) {mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}} <= return rec f { A?{l1(1).A!{l2(1).mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}, l3(1).end}}} (w1 :1) :Handler(A?{l1(1).A!{l2(1).mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}, l3(1).end}}) {mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}} . return handler A { l1(w2: 1) : A!{l2(1).mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}, l3(1).end} |-> let y :1 <= A!l3(()) in return () } in let hh :Handler(A?{l1(1).A!{l2(1).mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}, l3(1).end}}) <= [h ()] in suspend hh");
                "let h: " + fts + " <= return rec f {" + in1us + "} (w1 :1): " + h1s + "{" + recXBs
                        + "} . return handler A { {" + out2us + "} z1: Int, l1(w2: 1) |-> let y :1 <= A!l3(()) in return z1 } "
                        + "in let hh: " + h1s + " <= [h ()] in suspend hh 42");

        System.out.println(leth);
        leth.type(new GammaState(EAVIntType.INT), recXB);

        //--------------
        // config < B, idle, c[B] |-> let h = ... in ... >
        System.out.println();

		/*LinkedHashMap<EAName, EAValType> map = new LinkedHashMap<>();
		map.put(x, tf.val.unit());
		Gamma gamma = new Gamma(map, new LinkedHashMap<>());
		System.out.println("Typing hB: " + hsB1.type(gamma));

		LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaB = new LinkedHashMap<>();
		sigmaB.put(new Pair<>(s, B), hsB1);  // !!! TODO make sigma concrete, e.g., for typing
		EAPConfig cB = rf.config(p2, rf.idle(), sigmaB);*/

        EATActive tB = rf.activeThread(leth, s, B);
        LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigmaB = new LinkedHashMap<>();
        /*LinkedHashMap<Pair<EAPSid, Role>, Integer> stateB = new LinkedHashMap<>();
        stateB.put(new Pair<>(s, B), 0);
        EAPConfig cB = rf.config(p2, tB, sigmaB, stateB);*/
        EACActor cB = rf.config(p2, tB, sigmaB, pf.factory.intt(0));

        env = new LinkedHashMap<>();
        env.put(new Pair<>(s, B), recXB);
        System.out.println("Typing cB: " + cB + " ,, " + env);
        cB.type(new Gamma(), new Delta(env));
        //*/

        // ----

        System.out.println("\n---");
        System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);

        LinkedHashMap<EAPid, EACActor> cs = new LinkedHashMap<>();
        cs.put(p1, cA);
        cs.put(p2, cB);

        env = new LinkedHashMap<>();
        env.put(new Pair<>(s, A), out1u);
        //env.put(new Pair<>(s, B), in1u);  // !!! cf. EAPSystem this.annots.map.get(k2) -- use unfolded as annot -- XXX that only allows that many number of unfoldings during execution
        env.put(new Pair<>(s, B), recXB);
        System.out.println(env);
        EASystem sys = rf.system(lf, new Delta(env), cs);
        System.out.println(sys);
		/*Map<EAPPid, EAPConfig> cfgs = sys.getConfigs();
		//System.out.println("Typing p1/A: " + cfgs.get(p1));
		//cfgs.get(p1).type(new Gamma(), new Delta(env));  // TODO env for p1/A
		System.out.println("Typing p2/B: " + cfgs.get(p2));
		cfgs.get(p2).type(new Gamma(), new Delta(env));*/
        //env.put(new Pair<>(s, A), out1);
        //System.out.println(env);
        ////sys.type(new Gamma(), new Delta(), new Delta(env));
        sys.type(new Gamma(), new Delta());

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

        EACActor cA = RF.config(p1, RF.activeThread(lethA, s, A), newLinkedMap(), MF.factory.intt(0));
        EACActor cB = RF.config(p2, RF.activeThread(leth, s, B), newLinkedMap(), MF.factory.intt(0));
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

        Delta delta = new Delta(newLinkedMap(List.of(sA, sB), List.of(out1u, recXB)));
        EASystem sys = RF.system(LF, delta, newLinkedMap(List.of(p1, p2), List.of(cA, cB)));
        // !!! cf. EAPSystem this.annots.map.get(k2) -- use unfolded as annot -- XXX that only allows that many number of unfoldings during execution

        typeAndRun(sys, 100);
    }

    private static void ex2() {
        System.out.println("\n--ex2:");

        EALOutType out1 = (EALOutType) parseSessionType("B!{l1(1).B!{l2(1).end}}");
        EAMLet let = (EAMLet) parseM("let x: 1 <= B!l1(()) in B!l2(())");

        EALInType in1 = (EALInType) parseSessionType("A?{l1(1).A?{l2(1).end}}");
        EATIdle idle = RF.idle();
        EAEHandlers hsB1 = (EAEHandlers) parseV(
                "handler A {"
                        + "  {A?{l2(1).end}} d:Int, l1(x: 1) |-> suspend (handler A { {end} z:Int, l2(x: 1) |-> return z }) 42 "
                        + "}");

        // TODO factor out a Sigma class, e.g., for typing
        EACActor cA = RF.config(p1, RF.activeThread(let, s, A), newLinkedMap(), MF.unit());
        EACActor cB = RF.config(p2, idle, newLinkedMap(sB, hsB1), MF.intt(0));  // B step after active suspend
        System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);

        // ----

        /*System.out.println("Typing eA: " + out1 + " ,, " + let.type(new GammaState(EAVUnitType.UNIT), out1));
        LinkedHashMap<EAName, EAVType> map = newLinkedMap(x, TF.val.unit());
        GammaState gamma = new GammaState(map, newLinkedMap(), EAVIntType.INT);
        System.out.println("Typing hB: " + hsB1.type(gamma));*/

        typeCheckActor(cA, new Delta(newLinkedMap(sA, out1)));
        typeCheckActor(cB, new Delta(newLinkedMap(sB, in1)));

        // ----

        Delta delta = new Delta(newLinkedMap(List.of(sA, sB), List.of(out1, in1)));
        EASystem sys = RF.system(LF, delta, newLinkedMap(List.of(p1, p2), List.of(cA, cB)));
        typeAndRun(sys, -1);
    }
    //*/

    //*
    private static void ex1() {
        System.out.println("\n---ex1:");

        EALOutType out1 = (EALOutType) parseSessionType("B!{l1(Int).end}");
        EAMLet sendAB = (EAMLet) parseM("let x: Int <= return 41 + 1 in B!l1(x)");

        EALInType in1 = (EALInType) parseSessionType("A?{l1(Int).end}");
        EAEHandlers hsB = (EAEHandlers) parseV("handler A { {end} d: 1, l1(x: Int) |-> return d }");

        EACActor cA = RF.config(p1, RF.activeThread(sendAB, s, A), newLinkedMap(), MF.unit());
        EACActor cB = RF.config(p2, RF.idle(), newLinkedMap(sB, hsB), MF.unit());  // B step after active suspend
        System.out.println("cA: " + cA);
        System.out.println("cB: " + cB);

        // ---

        /*typeCheckComp(new GammaState(EAVUnitType.UNIT), sendAB, out1);
        GammaState gamma = new GammaState(newLinkedMap(x, TF.val.unit()), new LinkedHashMap<>(), EAVUnitType.UNIT);
        typeCheckExpr(gamma, hsB, TF.val.handlers(in1, TF.val.unit()));*/

        typeCheckActor(cA, new Delta(newLinkedMap(sA, out1)));
        typeCheckActor(cB, new Delta(newLinkedMap(sB, in1)));

        // ---

        LinkedHashMap<EAPid, EACActor> cs = newLinkedMap(List.of(p1, p2), List.of(cA, cB));
        LinkedHashMap<Pair<EASid, Role>, EALType> env = newLinkedMap(List.of(sA, sB), List.of(out1, in1));
        EASystem sys = RF.system(LF, new Delta(env), cs);
        typeAndRun(sys, -1);
    }
    //*/

    static <K, V> LinkedHashMap<K, V> newLinkedMap() {
        return new LinkedHashMap<>();
    }

    static <K, V> LinkedHashMap<K, V> newLinkedMap(K k, V v) {
        return newLinkedMap(List.of(k), List.of(v));
    }

    static <K, V> LinkedHashMap<K, V> newLinkedMap(List<K> ks, List<V> vs) {
        LinkedHashMap<K, V> res = new LinkedHashMap<>();
        Iterator<V> iv = vs.iterator();
        ks.forEach(x -> res.put(x, iv.next()));
        return res;
    }

    /* ... */

    static void typeCheckComp(GammaState gamma, EAComp M, EALType pre) {
        System.out.print("Typing computation: " + M);
        Either<Exception, Pair<Pair<EAVType, EALType>, Tree<String>>> typingA = M.type(gamma, pre);
        if (typingA.isLeft()) {
            throw new RuntimeException(typingA.getLeft().get());
        }
        Pair<Pair<EAVType, EALType>, Tree<String>> ppA = typingA.getRight().get();
        System.out.println(" " + pre + " ,, " + ppA.left + "\n" + ppA.right);
    }

    static void typeCheckExpr(GammaState gamma, EAExpr e, EAVType A) {
        System.out.print("Typing expr: " + e);
        Either<Exception, Pair<EAVType, Tree<String>>> t = e.type(gamma);
        if (t.isLeft()) {
            throw new RuntimeException(t.getLeft().get());
        }
        Pair<EAVType, Tree<String>> p = t.getRight().get();
        System.out.println(" " + p.left + "\n" + p.right);
        if (!p.left.equals(A)) {
            throw new RuntimeException("Type checking error, expected: " + A);
        }
    }

    static void typeCheckActor(EACActor c, Delta delta) {
        System.out.println("Typing actor: " + c + " ,, " + delta);
        c.type(new Gamma(), delta);
    }

    /* ... */

    static void typeAndRun(EASystem sys, int steps) {
        typeAndRun(sys, steps, false);
    }

    // steps -1 for unbounded
    static void typeAndRun(EASystem sys, int steps, boolean debug) {
        System.out.println("\nInitial system:\n" + sys);
        sys.type(new Gamma(), new Delta());

        int rem = steps;
        Map<EAPid, Set<EAPid>> pids = sys.canStep();
        for (; !pids.isEmpty() && rem != 0; rem--) {
            sys = sys.reduce(pids.keySet().iterator().next());  // FIXME HERE HERE always first act  // keyset is can-step-pids, (currently unused) Set is "partners"
            if (debug) {
                System.out.println();
                System.out.println(sys);
            }
            sys.type(new Gamma(), new Delta());
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


    /* parsing */

    static void testParser() {
        EATermFactory pf = EATermFactory.factory;
        EARuntimeFactory rf = EARuntimeFactory.factory;
        EATypeFactory tf = EATypeFactory.factory;

        //String input = "(A ! a((())))";
        String input = "let x: 1 <= A ! a((())) in suspend "
                + "(handler A {b(x : 1) : A?{b(1).C!{c(1).end}} -> return (),c(y: 1) : end -> return ()})";
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
