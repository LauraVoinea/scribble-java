package org.scribble.ext.ea.cli;

import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.local.LTypeFactory;
import org.scribble.core.type.session.local.LTypeFactoryImpl;
import org.scribble.ext.ea.core.runtime.*;
import org.scribble.ext.ea.core.runtime.config.EACActor;
import org.scribble.ext.ea.core.term.EATermFactory;
import org.scribble.ext.ea.core.term.comp.EAComp;
import org.scribble.ext.ea.core.term.comp.EAMLet;
import org.scribble.ext.ea.core.term.expr.EAEAPName;
import org.scribble.ext.ea.core.term.expr.EAEHandlers;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.*;
import org.scribble.ext.ea.core.type.value.EAVHandlersType;
import org.scribble.ext.ea.core.type.value.EAVIntType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.ConsoleColors;
import org.scribble.ext.ea.util.EAUtil;
import org.scribble.ext.ea.util.Either;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;


public class EATest {

    static final LinkedHashMap<Pair<EASid, Role>, EAEHandlers> EMPTY_SIGMA = EAUtil.mapOf();
    static final LinkedHashMap<EAIota, EAComp> EMPTY_RHO = EAUtil.mapOf();

    static final LTypeFactoryImpl LF = new LTypeFactoryImpl();

    //static final EATypeFactory TF = EATypeFactory.factory;
    static final EATermFactory MF = EATermFactory.factory;
    static final EARuntimeFactory RF = EARuntimeFactory.factory;

    static final Role A = new Role("A");
    static final Role B = new Role("B");
    static final Role B1 = new Role("B1");
    static final Role B2 = new Role("B2");
    static final EASid s = RF.sid("s");
    static final EAPid p1 = RF.pid("p1");
    static final EAPid p2 = RF.pid("p2");
    //static final EAEVar x = MF.var("x");
    static final EAEAPName c = MF.ap("c");

    static final Pair<EASid, Role> sA = Pair.of(s, A);
    static final Pair<EASid, Role> sB = Pair.of(s, B);

    public EATest() {
    }


    /* ... */

    public static void tests() {

        //System.out.println(EACommandLine.parseV("2 + 3"));

        String log = "";

        Map<String, Function<Boolean, Either<Exception, Pair<Set<EAAsyncSystem>, Set<EAAsyncSystem>>>>>
                tests = EAUtil.mapOf();
        //origTests(tests);
        //tests.put("ex11", EATest::ex11);
        //tests.put("ex12", EATest::ex12);
        tests.put("ex13", EATest::ex13);

        for (Map.Entry<String, Function<Boolean, Either<Exception, Pair<Set<EAAsyncSystem>, Set<EAAsyncSystem>>>>> e : tests.entrySet()) {
            String name = e.getKey();
            log = log(log, name, runTest(name, e.getValue(), true));
        }

        System.out.println("\nSummary:" + log);
    }

    private static void origTests(
            Map<String, Function<Boolean, Either<Exception, Pair<Set<EAAsyncSystem>, Set<EAAsyncSystem>>>>> tests) {
        tests.put("ex1", EATest::ex1);
        tests.put("ex2", EATest::ex2);
        tests.put("ex4", EATest::ex4);  // TODO rec bounding -- cf. repeat state bounding (OK for finite output seqs)
        tests.put("ex5", EATest::ex5);
        tests.put("ex6", EATest::ex6);
        tests.put("ex7", EATest::ex7);
        tests.put("ex8", EATest::ex8);
        tests.put("ex10", EATest::ex10);
    }

    private static void negtests() {
        LTypeFactoryImpl lf = new LTypeFactoryImpl();

        EATermFactory pf = EATermFactory.factory;
        EARuntimeFactory rf = EARuntimeFactory.factory;
        EATypeFactory tf = EATypeFactory.factory;

        //ex9(lf, pf, rf, tf);  // TODO neg runTest (check FAIL)
    }


    /* ... */

    private static Either<Exception, Pair<Set<EAAsyncSystem>, Set<EAAsyncSystem>>> ex13(
            boolean debug) {

        EAComp reg1 = EACommandLine.parseM("register c A return ()");
        EAComp reg2 = EACommandLine.parseM("register c B return ()");

        EACActor a1 = RF.actor(p1, RF.noSessionThread(reg1), EMPTY_SIGMA, EMPTY_RHO, MF.unit());
        EACActor a2 = RF.actor(p2, RF.noSessionThread(reg2), EMPTY_SIGMA, EMPTY_RHO, MF.unit());
        System.out.println("Actor " + a1.pid + " = " + a1);
        System.out.println("Actor " + a2.pid + " = " + a2);

        //--------------

        //EACommandLine.typeCheckActor(cB, new Delta(EAUtil.mapOf(sB, EALEndType.END)));
        EACommandLine.typeCheckActor(a1, new Delta());
        EACommandLine.typeCheckActor(a2, new Delta());

        // ----

        //Delta delta = new Delta(EAUtil.mapOf(sA, EALEndType.END, sB, EALEndType.END));
        Delta delta = new Delta();
        LinkedHashMap<EAPid, EACActor> cs = EAUtil.mapOf(a1.pid, a1, a2.pid, a2);
        //LinkedHashMap<EASid, EAGlobalQueue> queues = EAUtil.mapOf(s, new EAGlobalQueue(s));
        LinkedHashMap<EASid, EAGlobalQueue> queues = EAUtil.mapOf();
        LinkedHashMap<EAEAPName, Map<Role, Pair<EALType, List<EAIota>>>> access =
                EAUtil.mapOf(c, EAUtil.mapOf(
                        A, Pair.of(EALEndType.END, EAUtil.listOf()),
                        B, Pair.of(EALEndType.END, EAUtil.listOf())));  // // FIXME >=2 roles
        AsyncDelta adelta = new AsyncDelta(EAUtil.copyOf(delta.map), EAUtil.mapOf());
        EAAsyncSystem sys = RF.asyncSystem(LF, cs, queues, access, adelta);

        return EACommandLine.typeAndRun(sys, true, new EACommandLine.Bounds(-1));
    }

    private static Either<Exception, Pair<Set<EAAsyncSystem>, Set<EAAsyncSystem>>> ex12(
            boolean debug) {

        EAComp reg = EACommandLine.parseM("register c B2 return ()");
        //EACActor cA = RF.actor(p1, RF.noSessionThread(spawn), EMPTY_SIGMA, EMPTY_RHO, MF.unit());
        EACActor cB = RF.actor(p2, RF.noSessionThread(reg), EMPTY_SIGMA, EMPTY_RHO, MF.unit());
        //System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);

        //--------------

        //EACommandLine.typeCheckActor(cA, new Delta());
        //EACommandLine.typeCheckActor(cB, new Delta(EAUtil.mapOf(sB, EALEndType.END)));
        EACommandLine.typeCheckActor(cB, new Delta());

        // ----

        //Delta delta = new Delta(EAUtil.mapOf(sA, EALEndType.END, sB, EALEndType.END));
        Delta delta = new Delta();
        //LinkedHashMap<EAPid, EACActor> cs = EAUtil.mapOf(cA.pid, cA, cB.pid, cB);
        LinkedHashMap<EAPid, EACActor> cs = EAUtil.mapOf(cB.pid, cB);
        //LinkedHashMap<EASid, EAGlobalQueue> queues = EAUtil.mapOf(s, new EAGlobalQueue(s));
        LinkedHashMap<EASid, EAGlobalQueue> queues = EAUtil.mapOf();
        //LinkedHashMap<EAEAPName, Map<Role, List<EAIota>>> access = EAUtil.mapOf(c, EAUtil.mapOf());  // XXX all roles needed
        LinkedHashMap<EAEAPName, Map<Role, Pair<EALType, List<EAIota>>>> access =
                EAUtil.mapOf(c, EAUtil.mapOf(B2, Pair.of(EALEndType.END, EAUtil.listOf())));  // XXX all roles needed  // FIXME >=2 roles
        //AsyncDelta adelta = new AsyncDelta(EAUtil.copyOf(delta.map), EAUtil.mapOf(s, EAUtil.listOf()));
        AsyncDelta adelta = new AsyncDelta(EAUtil.copyOf(delta.map), EAUtil.mapOf());
        EAAsyncSystem sys = RF.asyncSystem(LF, cs, queues, access, adelta);

        return EACommandLine.typeAndRun(sys, true, new EACommandLine.Bounds(-1));
    }

    private static Either<Exception, Pair<Set<EAAsyncSystem>, Set<EAAsyncSystem>>> ex11(
            boolean debug) {

        EAComp spawn = EACommandLine.parseM("spawn return ()");

        EACActor cA = RF.actor(p1, RF.noSessionThread(spawn), EMPTY_SIGMA, EMPTY_RHO, MF.unit());
        //EACActor cB = RF.actor(p2, RF.noSessionThread(reg), EMPTY_SIGMA, EMPTY_RHO, MF.unit());
        System.out.println("cA = " + cA);
        //System.out.println("cB = " + cB);

        //--------------

        //EACommandLine.typeCheckActor(cA, new Delta(EAUtil.mapOf(sA, EALEndType.END)));
        EACommandLine.typeCheckActor(cA, new Delta());
        //EACommandLine.typeCheckActor(cB, new Delta());

        // ----

        //Delta delta = new Delta(EAUtil.mapOf(sA, EALEndType.END, sB, EALEndType.END));
        Delta delta = new Delta();
        LinkedHashMap<EAPid, EACActor> cs = EAUtil.mapOf(cA.pid, cA); //, cB.pid, cB);
        //LinkedHashMap<EASid, EAGlobalQueue> queues = EAUtil.mapOf(s, new EAGlobalQueue(s));
        LinkedHashMap<EASid, EAGlobalQueue> queues = EAUtil.mapOf();
        //AsyncDelta adelta = new AsyncDelta(EAUtil.copyOf(delta.map), EAUtil.mapOf(s, EAUtil.listOf()));
        AsyncDelta adelta = new AsyncDelta(EAUtil.copyOf(delta.map), EAUtil.mapOf());
        EAAsyncSystem sys = RF.asyncSystem(LF, cs, queues, EAUtil.mapOf(), adelta);

        return EACommandLine.typeAndRun(sys, true, new EACommandLine.Bounds(-1));
    }

    private static Either<Exception, Pair<Set<EAAsyncSystem>, Set<EAAsyncSystem>>> ex10(
            boolean debug) {

        EAComp lethA = EACommandLine.parseM("return 42");
        EAComp lethB = EACommandLine.parseM("return 43");

        EACActor cA = RF.actor(p1, RF.sessionThread(lethA, s, A), EMPTY_SIGMA, EMPTY_RHO, MF.intt(1));
        EACActor cB = RF.actor(p2, RF.sessionThread(lethB, s, B), EMPTY_SIGMA, EMPTY_RHO, MF.intt(2));
        System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);

        //--------------

        EACommandLine.typeCheckActor(cA, new Delta(EAUtil.mapOf(sA, EALEndType.END)));
        EACommandLine.typeCheckActor(cB, new Delta(EAUtil.mapOf(sB, EALEndType.END)));

        // ----

        Delta delta = new Delta(EAUtil.mapOf(sA, EALEndType.END, sB, EALEndType.END));
        LinkedHashMap<EAPid, EACActor> cs = EAUtil.mapOf(cA.pid, cA, cB.pid, cB);

        //EASystem sys = RF.system(LF, delta, EAUtil.mapOf(cA.pid, cA, cB.pid, cB));
        LinkedHashMap<EASid, EAGlobalQueue> queues = EAUtil.mapOf(s, new EAGlobalQueue(s));
        LinkedHashMap<EAEAPName, Map<Role, Pair<EALType, List<EAIota>>>> access = EAUtil.mapOf();
        AsyncDelta adelta = new AsyncDelta(EAUtil.copyOf(delta.map), EAUtil.mapOf(s, EAUtil.listOf()));
        EAAsyncSystem sys = RF.asyncSystem(LF, cs, queues, access, adelta);

        return EACommandLine.typeAndRun(sys, true, new EACommandLine.Bounds(-1));
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
        EAMLet lethA = (EAMLet) EACommandLine.parseM(
                "let h: " + h2s + " <= return handler A {"
                        + "{end} d: Int, l2(x: 1) |-> let w: Bool <= return d < 42 in return ()"
                        + "}"  // role hardcoded -- or state not accessible
                        + " in let g: 1 <= B!l1(h) in B!l2(())"
        );

        //---------------
        // config < A, idle, c[A] |-> let h = ... in ... >
        EATSession tA = rf.sessionThread(lethA, s, A);
        LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigmaA = new LinkedHashMap<>();
        EACActor cA = rf.actor(p1, tA, sigmaA, EMPTY_RHO, pf.factory.intt(0));

        System.out.println();
        String out1s = "B!{l1(" + h2s + ").B!{l2(1). end }}";
        EALOutType out1 = (EALOutType) EACommandLine.parseSessionType(out1s);
        LinkedHashMap<Pair<EASid, Role>, EALType> env = new LinkedHashMap<>();
        env.put(new Pair<>(s, A), out1);
        System.out.println("Typing cA: " + cA + " ,, " + env);
        cA.type(new Gamma(), new Delta(env));

        //---------------

        String in1s = "A?{l1(" + h2s + ")." + in2s + "}";
        String hBs = "Handler(Bool, " + in1s + ")";
        EAMLet lethB = (EAMLet) EACommandLine.parseM(
                "let h: " + hBs + " <= return handler A { {" + in2s + "} d: Bool, l1(x: " + h2s + ") |-> "
                        + " suspend x false }"  // suspend received x handler, XXX (data) type preservation -- d type Int at A, Bool at B
                        + " in suspend h true"
        );

        //--------------
        // config < B, idle, c[B] |-> let h = ... in ... } >
        EATSession tB = rf.sessionThread(lethB, s, B);
        LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigmaB = new LinkedHashMap<>();
        EACActor cB = rf.actor(p2, tB, sigmaB, EMPTY_RHO, pf.factory.bool(false));

        System.out.println();
        env = new LinkedHashMap<>();
        EALInType in1 = (EALInType) EACommandLine.parseSessionType(in1s);
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

        EACommandLine.typeAndRunOld(sys, -1);
        //
    }

    // N.B. slow with full debug output (very slow if no repeat state pruning)
    // Various options for B in comments
    private static Either<Exception, Pair<Set<EAAsyncSystem>, Set<EAAsyncSystem>>> ex8(
            boolean debug) {

        String recXAs = "mu X.B?{l2(1).B!{l1(1).X}, l3(1).end }";
        String out1us = "B!{l1(1)." + recXAs + "}";
        String in2us = "B?{l2(1)." + out1us + ", l3(1).end}";  // unfolding of recXA

        String h2s = "Handler (Int, " + in2us + ")";
        String hts = "{" + recXAs + "} 1 -> " + h2s + "{" + recXAs + "}";
        EAMLet lethA = (EAMLet) EACommandLine.parseM(
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
        EAMLet leth = (EAMLet) EACommandLine.parseM(
                "let h: " + htsB + " <= return"
                        + "  (rec f {  " + in1us + "} (w1: 1):" + h1s + "{" + recXBs + "} . return handler A {"
                        + "    {" + out2mus + "} d: Int, l1(w2: 1) "

                        /*//+ " |-> let y: 1 <= A!l2(()) in let z : " + h1s + " <= [f ()] in suspend z 42 })"  // run forever -- old
                        + " |-> let y: 1 <= A!l3(()) in return () })"  // quit straight away*/

                        //+ "     |-> let tmp: Bool <= < d 0 in "  // quit straight away -- 0
                        //+ "     |-> let tmp: Bool <= < d 42 in "  // quit after one -- 42
                        + "     |-> let tmp: Bool <= < d 43 in "  // run "forever" -- change run(-1) below -- XXX seen-pruned

                        + "        if tmp then let y: 1 <= A!l2(()) in let z : " + h1s + " <= [f ()] in suspend z 42"
                        + "        else let y: 1 <= A!l3(()) in return d"

                        + "  }) "
                        + "in let hh : " + h1s + " <= [h ()] in suspend hh 0");

        EACActor cA = RF.actor(p1, RF.sessionThread(lethA, s, A), EMPTY_SIGMA, EMPTY_RHO, MF.factory.intt(0));
        EACActor cB = RF.actor(p2, RF.sessionThread(leth, s, B), EMPTY_SIGMA, EMPTY_RHO, MF.factory.intt(0));
        System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);

        //---------------

        EALOutType out1u = (EALOutType) EACommandLine.parseSessionType(out1us);
        EALRecType recXB = (EALRecType) EACommandLine.parseSessionType(recXBs);

        /*lethA.type(new GammaState(EAVIntType.INT), out1u);
        leth.type(new GammaState(EAVIntType.INT), recXB);*/

        EACommandLine.typeCheckActor(cA, new Delta(EAUtil.mapOf(sA, out1u)));
        EACommandLine.typeCheckActor(cB, new Delta(EAUtil.mapOf(sB, recXB)));

        // ----

        Delta delta = new Delta(EAUtil.mapOf(sA, out1u, sB, recXB));
        LinkedHashMap<EAPid, EACActor> cs = EAUtil.mapOf(cA.pid, cA, cB.pid, cB);

        //EASystem sys = RF.system(LF, delta, EAUtil.mapOf(cA.pid, cA, cB.pid, cB));
        LinkedHashMap<EASid, EAGlobalQueue> queues = EAUtil.mapOf(s, new EAGlobalQueue(s));
        LinkedHashMap<EAEAPName, Map<Role, Pair<EALType, List<EAIota>>>> access = EAUtil.mapOf();
        AsyncDelta adelta = new AsyncDelta(EAUtil.copyOf(delta.map), EAUtil.mapOf(s, EAUtil.listOf()));
        EAAsyncSystem sys = RF.asyncSystem(LF, cs, queues, access, adelta);

        return EACommandLine.typeAndRun(sys, true, new EACommandLine.Bounds(-1));  // quit straight away or after one -- also "run forever" (but repeat state bounded -- cf. no unbounded send stream)
        //return EACommandLine.typeAndRunD(sys, true, new EACommandLine.Bounds(10));  // run forever  // XXX repeat state is now also bounded
    }

    private static Either<Exception, Pair<Set<EAAsyncSystem>, Set<EAAsyncSystem>>> ex7(
            boolean debug) {

        String recXAs = "mu X.B?{l2(1).B!{l1(1).X, l4(1).end}, l3(1).end }";
        String out1us = "B!{l1(1)." + recXAs + ", l4(1).end}";
        String in2us = "B?{l2(1)." + out1us + ", l3(1).end}";  // unfolding of recXA
        String h2s = "Handler (Int, " + in2us + ")";

        String hts = "{" + recXAs + "} 1 -> " + h2s + " {" + recXAs + "}";
        EAMLet lethA = (EAMLet) EACommandLine.parseM(
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
        EAMLet leth = (EAMLet) EACommandLine.parseM(
                "let h: " + htsB + " <= return"
                        + "  (rec f{  " + in1us + "} (w1: 1):" + h1s + "{" + recXBs + "} . return handler A {"
                        + "    {" + out2mus + "} d: Int, l1(w2: 1) |->"
                        + "      let y: 1 <= A!l2(()) in let z : " + h1s + " <= [f ()] in suspend z 42,"
                        + "    {end} d: Int, l4(w4: 1) |-> return d"
                        + "  })"
                        + "in let hh : " + h1s + " <= [h ()] in suspend hh 0");

        EACActor cA = RF.actor(p1, RF.sessionThread(lethA, s, A), EMPTY_SIGMA, EMPTY_RHO, MF.factory.intt(0));
        EACActor cB = RF.actor(p2, RF.sessionThread(leth, s, B), EMPTY_SIGMA, EMPTY_RHO, MF.factory.intt(0));
        System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);

        //---------------

        EALOutType out1u = (EALOutType) EACommandLine.parseSessionType(out1us);
        EALRecType recXB = (EALRecType) EACommandLine.parseSessionType(recXBs);

        /*lethA.type(new GammaState(EAVIntType.INT), out1u);
        leth.type(new GammaState(EAVIntType.INT), recXB);*/

        EACommandLine.typeCheckActor(cA, new Delta(EAUtil.mapOf(sA, out1u)));
        EACommandLine.typeCheckActor(cB, new Delta(EAUtil.mapOf(sB, recXB)));

        // ----

        Delta delta = new Delta(EAUtil.mapOf(sA, out1u, sB, recXB));
        LinkedHashMap<EAPid, EACActor> cs = EAUtil.mapOf(cA.pid, cA, cB.pid, cB);

        //EASystem sys = RF.system(LF, delta, EAUtil.mapOf(cA.pid, cA, cB.pid, cB));
        LinkedHashMap<EASid, EAGlobalQueue> queues = EAUtil.mapOf(s, new EAGlobalQueue(s));
        LinkedHashMap<EAEAPName, Map<Role, Pair<EALType, List<EAIota>>>> access = EAUtil.mapOf();
        AsyncDelta adelta = new AsyncDelta(EAUtil.copyOf(delta.map), EAUtil.mapOf(s, EAUtil.listOf()));
        EAAsyncSystem sys = RF.asyncSystem(LF, cs, queues, access, adelta);

        return EACommandLine.typeAndRun(sys, true, new EACommandLine.Bounds(-1));  // binary recip, implicitly bounded
    }

    private static Either<Exception, Pair<Set<EAAsyncSystem>, Set<EAAsyncSystem>>> ex6(
            boolean debug) {

        EALOutType out1 = (EALOutType) EACommandLine.parseSessionType("B!{l1(Bool).end}");
        EAComp sendAB = EACommandLine.parseM("let x: Bool <= < 2 2 in if x then B!l1(x) else B!l1(x)");

        EALInType in1 = (EALInType) EACommandLine.parseSessionType("A?{l1(Bool).end}");
        EAEHandlers hsB = (EAEHandlers) EACommandLine.parseV("handler A { {end} z1: 1, l1(x: Bool) |-> return 42 }");

        EACActor cA = RF.actor(p1, RF.sessionThread(sendAB, s, A), EMPTY_SIGMA, EMPTY_RHO, MF.unit());
        EACActor cB = RF.actor(p2, RF.idleThread(), EAUtil.mapOf(sB, hsB), EMPTY_RHO, MF.intt(0));
        System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);

        // ---

        //System.out.println("Typing eA: " + out1 + " ,, " + sendAB.type(new GammaState(EAVIntType.INT), out1));

        EACommandLine.typeCheckActor(cA, new Delta(EAUtil.mapOf(sA, out1)));
        EACommandLine.typeCheckActor(cB, new Delta(EAUtil.mapOf(sB, in1)));

        // ---

        Delta delta = new Delta(EAUtil.mapOf(sA, out1, sB, in1));
        LinkedHashMap<EAPid, EACActor> cs = EAUtil.mapOf(cA.pid, cA, cB.pid, cB);

        //EASystem sys = RF.system(LF, delta, EAUtil.mapOf(cA.pid, cA, cB.pid, cB));
        LinkedHashMap<EASid, EAGlobalQueue> queues = EAUtil.mapOf(s, new EAGlobalQueue(s));
        LinkedHashMap<EAEAPName, Map<Role, Pair<EALType, List<EAIota>>>> access = EAUtil.mapOf();
        AsyncDelta adelta = new AsyncDelta(EAUtil.copyOf(delta.map), EAUtil.mapOf(s, EAUtil.listOf()));
        EAAsyncSystem sys = RF.asyncSystem(LF, cs, queues, access, adelta);

        return EACommandLine.typeAndRun(sys, true, new EACommandLine.Bounds(-1));  // binary recip, implicitly bounded
    }

    private static Either<Exception, Pair<Set<EAAsyncSystem>, Set<EAAsyncSystem>>> ex5(
            boolean debug) {

        String recXAs = "mu X . B?{l2(1).B!{l1(1).X}, l3(1).end}";
        String out1us = "B!{l1(1)." + recXAs + "}";
        String in2us = "B?{l2(1)." + out1us + ", l3(1).end}";
        String h2s = "Handler(Int, " + in2us + ")";

        String ftAs = "{" + in2us + "} 1 -> " + h2s + " {" + recXAs + "}";
        EAMLet lethA = (EAMLet) EACommandLine.parseM(
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
        EAMLet leth = (EAMLet) EACommandLine.parseM(
                "let h: " + fts + " <= return"
                        + "  (rec f {" + in1us + "} (w1: 1): " + h1s + " {" + recXBs + "} . return handler A {"
                        + "    {" + out2us + "} d: Int, l1(w2: 1) |-> let y: 1 <= A!l3(()) in return d"
                        + "  }) "
                        + "in let hh: " + h1s + " <= [h ()] in suspend hh 43");

        EACActor cA = RF.actor(p1, RF.sessionThread(lethA, s, A), EMPTY_SIGMA, EMPTY_RHO, MF.intt(0));
        EACActor cB = RF.actor(p2, RF.sessionThread(leth, s, B), EMPTY_SIGMA, EMPTY_RHO, MF.intt(0));
        System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);

        // ---

        EALOutType out1u = (EALOutType) EACommandLine.parseSessionType(out1us);
        EALRecType recXB = (EALRecType) EACommandLine.parseSessionType(recXBs);

        /*lethA.type(new GammaState(EAVIntType.INT), out1u);
        leth.type(new GammaState(EAVIntType.INT), recXB);*/

        EACommandLine.typeCheckActor(cA, new Delta(EAUtil.mapOf(sA, out1u)));
        EACommandLine.typeCheckActor(cB, new Delta(EAUtil.mapOf(sB, recXB)));

        // ----

        Delta delta = new Delta(EAUtil.mapOf(sA, out1u, sB, recXB));
        LinkedHashMap<EAPid, EACActor> cs = EAUtil.mapOf(cA.pid, cA, cB.pid, cB);

        //EASystem sys = RF.system(LF, delta, cs);
        LinkedHashMap<EASid, EAGlobalQueue> queues = EAUtil.mapOf(s, new EAGlobalQueue(s));
        LinkedHashMap<EAEAPName, Map<Role, Pair<EALType, List<EAIota>>>> access = EAUtil.mapOf();
        AsyncDelta adelta = new AsyncDelta(EAUtil.copyOf(delta.map), EAUtil.mapOf(s, EAUtil.listOf()));
        EAAsyncSystem sys = RF.asyncSystem(LF, cs, queues, access, adelta);

        return EACommandLine.typeAndRun(sys, true, new EACommandLine.Bounds(-1));  // binary recip, implicitly bounded
    }

    // XXX DEBUG loop -- cf. ex5 OK
    private static void ex5bugged() {
        System.out.println("\n---ex5:");

        String recXAs = "mu X . B?{l2(1).B!{l1(1).X}, l3(1).end}";
        String out1us = "B!{l1(1).mu X . " + recXAs + "}";
        String in2us = "B?{l2(1)." + out1us + ", l3(1).end}";
        String h2s = "Handler(Int, " + in2us + ")";

        String ftAs = "{" + in2us + "} 1 -> " + h2s + "{" + recXAs + "}";
        EAMLet lethA = (EAMLet) EACommandLine.parseM(
                "let h : " + ftAs + " <= return rec f {" + in2us + "} (w1 :1) : " + h2s + " {" + recXAs
                        + "} . return handler B { {" + out1us + "} z2: Int, l2(w2: 1) |-> let y :1 <= B!l1(())"
                        + "in let z : " + h2s + " <= [f ()] in suspend z 42 ,"
                        + "{end} z3: Int, l3(w2: 1) |-> return z3 } "
                        + "in let w1 :1 <= B!l1(()) in let hh: " + h2s + " <= [h ()] in suspend hh 42");

        System.out.println(lethA);

        String out2s = "A!{ l2(1) . X, l3(1).end }";
        String in1s = "A?{ l1(1) . " + out2s + " }";
        String recXBs = "mu X . " + in1s;
        EALRecType recXB = (EALRecType) EACommandLine.parseSessionType(recXBs);

        String out2mus = "A!{l2(1) . " + recXBs + ", l3(1) . end }";

        String out2us = out2mus;
        String in1us = "A?{l1(1) . " + out2us + "}";

        String h1s = "Handler(Int, " + in1us + ")";
        EAVHandlersType h1 = (EAVHandlersType) EACommandLine.parseA(h1s);

        String fts = "{" + in1us + "} 1 -> " + h1s + "{" + recXBs + "}";
        EAMLet leth = (EAMLet) EACommandLine.parseM(
                //"let h : {A?{l1(1).A!{l2(1).mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}, l3(1).end}}}1 -> Handler(A?{l1(1).A!{l2(1).mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}, l3(1).end}}) {mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}} <= return rec f { A?{l1(1).A!{l2(1).mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}, l3(1).end}}} (w1 :1) :Handler(A?{l1(1).A!{l2(1).mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}, l3(1).end}}) {mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}} . return handler A { l1(w2: 1) : A!{l2(1).mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}, l3(1).end} |-> let y :1 <= A!l3(()) in return () } in let hh :Handler(A?{l1(1).A!{l2(1).mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}, l3(1).end}}) <= [h ()] in suspend hh");
                "let h: " + fts + " <= return rec f {" + in1us + "} (w1 :1): " + h1s + "{" + recXBs
                        + "} . return handler A { {" + out2us + "} z1: Int, l1(w2: 1) |-> let y :1 <= A!l3(()) in return z1 } "
                        + "in let hh: " + h1s + " <= [h ()] in suspend hh 42");

        // config < A, idle, c[A] |-> let h = ... in ... >
        EATSession tA = RF.sessionThread(lethA, s, A);
        LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigmaA = new LinkedHashMap<>();
        /*LinkedHashMap<Pair<EAPSid, Role>, Integer> stateA = new LinkedHashMap<>();
        stateA.put(new Pair<>(s, A), 0);
        EAPConfig cA = rf.config(p1, tA, sigmaA, stateA);*/
        EACActor cA = RF.actor(p1, tA, sigmaA, EMPTY_RHO, MF.intt(0));

        // config < B, idle, c[B] |-> let h = ... in ... >
        System.out.println();

		/*LinkedHashMap<EAName, EAValType> map = new LinkedHashMap<>();
		map.put(x, tf.val.unit());
		Gamma gamma = new Gamma(map, new LinkedHashMap<>());
		System.out.println("Typing hB: " + hsB1.type(gamma));

		LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaB = new LinkedHashMap<>();
		sigmaB.put(new Pair<>(s, B), hsB1);  // !!! TODO make sigma concrete, e.g., for typing
		EAPConfig cB = rf.config(p2, rf.idle(), sigmaB);*/

        EATSession tB = RF.sessionThread(leth, s, B);
        LinkedHashMap<Pair<EASid, Role>, EAEHandlers> sigmaB = new LinkedHashMap<>();
        /*LinkedHashMap<Pair<EAPSid, Role>, Integer> stateB = new LinkedHashMap<>();
        stateB.put(new Pair<>(s, B), 0);
        EAPConfig cB = rf.config(p2, tB, sigmaB, stateB);*/
        EACActor cB = RF.actor(p2, tB, sigmaB, EMPTY_RHO, MF.intt(0));

        System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);

        //---------------

        EALOutType out1u = (EALOutType) EACommandLine.parseSessionType(out1us);

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

        EACommandLine.typeAndRunOld(sys, -1);

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

    private static Either<Exception, Pair<Set<EAAsyncSystem>, Set<EAAsyncSystem>>> ex4(
            boolean debug) {

        String recXAs = "mu X.B?{l2(1).B!{l1(1).X}}";
        String out1us = "B!{l1(1)." + recXAs + "}";
        String in2us = "B?{l2(1)." + out1us + "}";  // unfolding of recXA
        String h2s = "Handler (Int, " + in2us + ")";

        String hts = "{" + in2us + "} 1 -> " + h2s + "{" + recXAs + "}";
        EAMLet lethA = (EAMLet) EACommandLine.parseM(
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
        EAMLet leth = (EAMLet) EACommandLine.parseM(
                "let h: " + htsB + " <= return"
                        + "  (rec f {  " + in1us + "} (w1: 1): " + h1s + "{" + recXBs + "} . return handler A {"
                        + "    {" + out2mus + "} d: Int, l1(w2: 1) |->"
                        + "      let y: 1 <= A!l2(()) in let z : " + h1s + " <= [f ()] in suspend z 42"
                        + "  }) "
                        + "in let hh : " + h1s + " <= [h ()] in suspend hh 42");

        EACActor cA = RF.actor(p1, RF.sessionThread(lethA, s, A), EMPTY_SIGMA, EMPTY_RHO, MF.intt(0));
        EACActor cB = RF.actor(p2, RF.sessionThread(leth, s, B), EMPTY_SIGMA, EMPTY_RHO, MF.intt(0));
        System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);

        //--------------

        EALOutType out1u = (EALOutType) EACommandLine.parseSessionType(out1us);
        EALRecType recXB = (EALRecType) EACommandLine.parseSessionType(recXBs);

        /*System.out.println(lethA);
        lethA.type(new GammaState(EAVIntType.INT), out1u);
        System.out.println(leth);
        leth.type(new GammaState(EAVIntType.INT), recXB);*/

        EACommandLine.typeCheckActor(cA, new Delta(EAUtil.mapOf(sA, out1u)));
        EACommandLine.typeCheckActor(cB, new Delta(EAUtil.mapOf(sB, recXB)));

        // ----

        Delta delta = new Delta(EAUtil.mapOf(sA, out1u, sB, recXB));
        LinkedHashMap<EAPid, EACActor> cs = EAUtil.mapOf(cA.pid, cA, cB.pid, cB);

        //EASystem sys = RF.system(LF, delta, cs);
        // !!! cf. EAPSystem this.annots.map.get(k2) -- use unfolded as annot -- XXX that only allows that many number of unfoldings during execution
        LinkedHashMap<EASid, EAGlobalQueue> queues = EAUtil.mapOf(s, new EAGlobalQueue(s));
        LinkedHashMap<EAEAPName, Map<Role, Pair<EALType, List<EAIota>>>> access = EAUtil.mapOf();
        AsyncDelta adelta = new AsyncDelta(EAUtil.copyOf(delta.map), EAUtil.mapOf(s, EAUtil.listOf()));
        EAAsyncSystem sys = RF.asyncSystem(LF, cs, queues, access, adelta);

        return EACommandLine.typeAndRun(sys, debug, new EACommandLine.Bounds(-1));  // binary recip, implicitly bounded
    }

    private static Either<Exception, Pair<Set<EAAsyncSystem>, Set<EAAsyncSystem>>> ex2(
            boolean debug) {

        EALOutType out1 = (EALOutType) EACommandLine.parseSessionType("B!{l1(1).B!{l2(1).end}}");
        EAMLet let = (EAMLet) EACommandLine.parseM("let x: 1 <= B!l1(()) in B!l2(())");

        EALInType in1 = (EALInType) EACommandLine.parseSessionType("A?{l1(1).A?{l2(1).end}}");
        EATIdle idle = RF.idleThread();
        EAEHandlers hsB1 = (EAEHandlers) EACommandLine.parseV(
                "handler A {"
                        + "  {A?{l2(1).end}} d: Int, l1(x: 1) |->"
                        + "    suspend (handler A { {end} z:Int, l2(x: 1) |-> return z }) 42 "
                        + "}");

        // TODO factor out a Sigma class, e.g., for typing
        EACActor cA = RF.actor(p1, RF.sessionThread(let, s, A), EMPTY_SIGMA, EMPTY_RHO, MF.unit());
        EACActor cB = RF.actor(p2, idle, EAUtil.mapOf(sB, hsB1), EMPTY_RHO, MF.intt(0));  // B step after active suspend
        /*System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);*/

        // ----

        /*EACommandLine.typeCheckActor(cA, new Delta(EAUtil.mapOf(sA, out1)));
        EACommandLine.typeCheckActor(cB, new Delta(EAUtil.mapOf(sB, in1)));*/

        // ----

        Delta delta = new Delta(EAUtil.mapOf(sA, out1, sB, in1));
        LinkedHashMap<EAPid, EACActor> cs = EAUtil.mapOf(cA.pid, cA, cB.pid, cB);

        //EASystem sys = RF.system(LF, delta, cs);
        LinkedHashMap<EASid, EAGlobalQueue> queues = EAUtil.mapOf(s, new EAGlobalQueue(s));
        LinkedHashMap<EAEAPName, Map<Role, Pair<EALType, List<EAIota>>>> access = EAUtil.mapOf();
        AsyncDelta adelta = new AsyncDelta(EAUtil.copyOf(delta.map), EAUtil.mapOf(s, EAUtil.listOf()));
        EAAsyncSystem sys = RF.asyncSystem(LF, cs, queues, access, adelta);

        //EACommandLine.typeAndRun(sys, -1, true);
        return EACommandLine.typeAndRun(sys, true, new EACommandLine.Bounds(-1));
    }
    //*/

    //*
    private static Either<Exception, Pair<Set<EAAsyncSystem>, Set<EAAsyncSystem>>> ex1(
            boolean debug) {

        EALOutType out1 = (EALOutType) EACommandLine.parseSessionType("B!{l1(Int).end}");
        //EAMLet sendAB = (EAMLet) EACommandLine.parseM("let x: Int <= return 41 + 1 in B!l1(x)");  // TODO refactor binop
        EAMLet sendAB = (EAMLet) EACommandLine.parseM("let x: Int <= + 41 1 in B!l1(x)");

        EALInType in1 = (EALInType) EACommandLine.parseSessionType("A?{l1(Int).end}");
        EAEHandlers hsB = (EAEHandlers) EACommandLine.parseV("handler A { {end} d: 1, l1(x: Int) |-> return d }");

        EACActor cA = RF.actor(p1, RF.sessionThread(sendAB, s, A), EMPTY_SIGMA, EMPTY_RHO, MF.unit());
        EACActor cB = RF.actor(p2, RF.idleThread(), EAUtil.mapOf(sB, hsB), EMPTY_RHO, MF.unit());  // B step after active suspend
        /*System.out.println("cA: " + cA);
        System.out.println("cB: " + cB);*/

        // ---

        /*EACommandLine.typeCheckActor(cA, new Delta(EAUtil.mapOf(sA, out1)));
        EACommandLine.typeCheckActor(cB, new Delta(EAUtil.mapOf(sB, in1)));*/

        // ---

        LinkedHashMap<EAPid, EACActor> cs = EAUtil.mapOf(cA.pid, cA, cB.pid, cB);
        LinkedHashMap<Pair<EASid, Role>, EALType> env = EAUtil.mapOf(sA, out1, sB, in1);

        Delta delta = new Delta(env);
        //EASystem sys = RF.system(LF, new Delta(env), cs);
        LinkedHashMap<EASid, EAGlobalQueue> queues = EAUtil.mapOf(s, new EAGlobalQueue(s));
        LinkedHashMap<EAEAPName, Map<Role, Pair<EALType, List<EAIota>>>> access = EAUtil.mapOf();
        AsyncDelta adelta = new AsyncDelta(EAUtil.copyOf(delta.map), EAUtil.mapOf(s, EAUtil.listOf()));
        EAAsyncSystem sys = RF.asyncSystem(LF, cs, queues, access, adelta);

        //EACommandLine.typeAndRun(sys, -1);
        //EACommandLine.typeAndRun(sys, -1, true);
        return EACommandLine.typeAndRun(sys, debug, new EACommandLine.Bounds(-1));
    }


    /* ... */

    private static boolean runTest(
            String name,
            Function<Boolean, Either<Exception, Pair<Set<EAAsyncSystem>, Set<EAAsyncSystem>>>> test,
            boolean debug) {

        System.out.println("\n\n-- " + name + ":\n");
        Either<Exception, Pair<Set<EAAsyncSystem>, Set<EAAsyncSystem>>> apply =
                test.apply(debug);
        if (apply.isLeft()) {
            apply.getLeft().printStackTrace();
            return false;
        }

        Pair<Set<EAAsyncSystem>, Set<EAAsyncSystem>> right = apply.getRight();
        System.out.println("\nDepth pruned:");
        for (EAAsyncSystem pruned : right.left) {
            System.out.println("\n" + pruned);
        }
        System.out.println("\nTerminals:");
        for (EAAsyncSystem term : right.right) {
            System.out.println("\n" + term);
        }

        return right.left.isEmpty();
    }

    static String log(String log, String name, boolean pass) {
        return log + "\n" + name + (pass ? " Pass" : ConsoleColors.colour(ConsoleColors.RED, " FAIL"));
    }
}
