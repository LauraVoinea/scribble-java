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
import org.scribble.ext.ea.core.config.*;
import org.scribble.ext.ea.core.process.*;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.*;
import org.scribble.ext.ea.core.type.value.EAFuncType;
import org.scribble.ext.ea.core.type.value.EAHandlersType;
import org.scribble.ext.ea.core.type.value.EAIntType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.parser.antlr.EACalculusLexer;
import org.scribble.ext.ea.parser.antlr.EACalculusParser;
import org.scribble.ext.ea.util.EAPPair;
import org.scribble.util.AntlrSourceException;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;


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

    static void testParser() {
        EAPFactory pf = EAPFactory.factory;
        EAPRuntimeFactory rf = EAPRuntimeFactory.factory;
        EATypeFactory tf = EATypeFactory.factory;

        //String input = "(A ! a((())))";
        String input = "let x: 1 <= A ! a((())) in suspend "
                + "(handler A {b(x : 1) : A?{b(1).C!{c(1).end}} -> return (),c(y: 1) : end -> return ()})";
        EAPTerm res = parseM(input);
        System.out.println("bbb: " + res);
    }

    static EAPExpr parseM(String input) {
        Lexer lex = new EACalculusLexer(new ANTLRStringStream(input));
        EACalculusParser par = new EACalculusParser(new CommonTokenStream(lex));
        try {
            //par.setTreeAdaptor(new EATreeAdaptor());  // XXX this requires nodes to be CommonTree to add children -- adaptor only constructs each node individualy without children
            CommonTree tree = (CommonTree) par.start().getTree();
            //System.out.println("aaa: " + tree.getClass() + "\n" + tree.getText() + " ,, " + tree.getChild(0) + " ,, " + tree.getChild(1));

            EAPExpr res = new EAFuncNamesFixer().parse(new EAASTBuilder().visitM((CommonTree) tree.getChild(0)));
            return res;

            //tree.token;
        } catch (RecognitionException x) {
            throw new RuntimeException(x);
        }
    }

    static EAValType parseA(String input) {
        Lexer lex = new EACalculusLexer(new ANTLRStringStream(input));
        EACalculusParser par = new EACalculusParser(new CommonTokenStream(lex));
        try {
            CommonTree tree = (CommonTree) par.type().getTree();
            return new EAASTBuilder().visitA(tree);
        } catch (RecognitionException x) {
            throw new RuntimeException(x);
        }
    }

    static EAPVal parseV(String input) {
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

    private static void eamain() {
        LTypeFactoryImpl lf = new LTypeFactoryImpl();

        EAPFactory pf = EAPFactory.factory;
        EAPRuntimeFactory rf = EAPRuntimeFactory.factory;
        EATypeFactory tf = EATypeFactory.factory;

        // HERE HERE merge rhu1-refactorinterfaces -- i.e., latest scrib-core

        //System.out.println(parseV("2 + 3"));

        //ex1(lf, pf, rf, tf);
        //ex2(lf, pf, rf, tf);
        //ex4(lf, pf, rf, tf);
        //ex5(lf, pf, rf, tf);
        //ex6(lf, pf, rf, tf);
        //ex7(lf, pf, rf, tf);
        ex8(lf, pf, rf, tf);

        /* HERE HERE
        - state is currently per sess -- probably should be config wide, state shared across sess -> (intentional) race conditions
        - ...take state on config creation, infer state type from that
        - ...need (self) handler firing when state satis some condition -- !!! how done in standard actors?
        - recursion example with if-then and state
        ...
        - refactor EAPVal as EAPPure -- separate packages for expr/pure -- distinguish actual val from pure
        - separate isGround/isValue -- cf. canBeta for exprs vs. is-stuck
        - fix canBeta contexts and isGround for all exprs
        - tidy foo vs. beta -- cf. some expr foo is just beta (some not, e.g., let)
        */


        //new EACommandLine(args).run();
    }

    static void ex8(LTypeFactory lf, EAPFactory pf, EAPRuntimeFactory rf, EATypeFactory tf) {
        Role A = new Role("A");
        Role B = new Role("B");
        EAPSid s = rf.sid("s");
        EAPPid p1 = rf.pid("p1");
        EAPPid p2 = rf.pid("p2");

        String out1s = "B!{l1(1).X}";
        String in2s = "B?{l2(1)." + out1s + ", l3(1).end }";
        String recXAs = "mu X." + in2s;
        String out1us = "B!{l1(1)." + recXAs + "}";
        String in2us = "B?{l2(1)." + out1us + ", l3(1).end}";  // unfolding of recXA

        // ----

        String h2s = "Handler (" + in2us + ")";
        String hts = "{" + recXAs + "} 1-> " + h2s + "{" + recXAs + "}";
        EAPLet lethA = (EAPLet) parseM(
                "let h: " + hts + " <= return (rec f { " + recXAs + "} (w1: 1 ):" + h2s + "{" + recXAs
                        + "} . return handler B { {" + out1us + "} z2: Int, l2(w2: 1) "
                        + " |-> let y: 1 <= B!l1(()) in let z : " + h2s + " <= [f ()] in suspend z, 42"
                        + ",  {end} z3: Int, l3(w3: 1) |-> return () })"
                        + "in let w3 : 1 <= B!l1(()) in let hh : " + h2s + " <= [h ()] in suspend hh, 0");

        System.out.println(lethA);
        EALOutType out1u = (EALOutType) parseSessionType(out1us);
        lethA.type(new Gamma(), out1u);

        //---------------
        // config < A, idle, c[A] |-> let h = ... in ... >
        System.out.println();

        EAPActiveThread tA = rf.activeThread(lethA, s, A);
        LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaA = new LinkedHashMap<>();
        EAPConfig<?> cA = rf.config(p1, tA, sigmaA, pf.factory.intt(0));

        LinkedHashMap<Pair<EAPSid, Role>, EALType> env = new LinkedHashMap<>();
        env.put(new EAPPair<>(s, A), out1u);
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

        String h1s = "Handler (" + in1us + ")";
        String htsB = "{" + in1us + "} 1 ->" + h1s + "{" + recXBs + "}";
        EAPLet leth = (EAPLet) parseM(
                "let h: " + htsB + " <= return (rec f{  " + in1us + "} (w1: 1):" + h1s + "{" + recXBs + "} ."
                        + "return handler A { {" + out2mus + "} d: Int, l1(w2: 1) "

                        /*//+ " |-> let y: 1 <= A!l2(()) in let z : " + h1s + " <= [f ()] in suspend z, 42 })"  // run forever
                        + " |-> let y: 1 <= A!l3(()) in return () })"*/  // quit straight away

                        //+ " |-> let tmp: Bool <= return d < 0 in "  // quit straight away
                        + " |-> let tmp: Bool <= return d < 1 in "  // quit after one
                        //+ " |-> let tmp: Bool <= return d < 43 in "  // run forever

                        + "if tmp then let y: 1 <= A!l2(()) in let z : " + h1s + " <= [f ()] in suspend z, 42"
                        + "else let y: 1 <= A!l3(()) in return ()"

                        + " })"
                        + "in let hh : " + h1s + " <= [h ()] in suspend hh, 0");

        System.out.println(leth);
        EALRecType recXB = (EALRecType) parseSessionType(recXBs);
        leth.type(new Gamma(), recXB);

        //--------------
        // config < B, idle, c[B] |-> let h = ... in ... } >
        System.out.println();

        EAPActiveThread tB = rf.activeThread(leth, s, B);
        LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaB = new LinkedHashMap<>();
        EAPConfig<?> cB = rf.config(p2, tB, sigmaB, pf.factory.intt(0));

        env = new LinkedHashMap<>();
        env.put(new EAPPair<>(s, B), recXB);
        System.out.println("Typing cB: " + cB + " ,, " + env);
        cB.type(new Gamma(), new Delta(env));
        //*/

        // ----

        System.out.println("\n---");
        System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);

        LinkedHashMap<EAPPid, EAPConfig<?>> cs = new LinkedHashMap<>();
        cs.put(cA.pid, cA);
        cs.put(cB.pid, cB);

        env.put(new EAPPair<>(s, A), out1u);
        env.put(new EAPPair<>(s, B), recXB);
        System.out.println(env);
        EAPSystem sys = rf.system(lf, new Delta(env), cs);
        System.out.println(sys);
        sys.type(new Gamma(), new Delta());

        run(sys, 100);
    }

    static void ex7(LTypeFactory lf, EAPFactory pf, EAPRuntimeFactory rf, EATypeFactory tf) {
        Role A = new Role("A");
        Role B = new Role("B");
        EAPSid s = rf.sid("s");
        EAPPid p1 = rf.pid("p1");
        EAPPid p2 = rf.pid("p2");

        String out1s = "B!{l1(1).X, l4(1).end}";
        String in2s = "B?{l2(1)." + out1s + ", l3(1).end }";
        String recXAs = "mu X." + in2s;
        String out1us = "B!{l1(1)." + recXAs + ", l4(1).end}";
        String in2us = "B?{l2(1)." + out1us + ", l3(1).end}";  // unfolding of recXA

        // ----

        String h2s = "Handler (" + in2us + ")";
        String hts = "{" + recXAs + "} 1-> " + h2s + "{" + recXAs + "}";
        EAPLet lethA = (EAPLet) parseM(
                "let h: " + hts + " <= return (rec f { " + recXAs + "} (w1: 1 ):" + h2s + "{" + recXAs
                        + "} . return handler B { {" + out1us + "} z2: Int, l2(w2: 1) "
                        + " |-> let y: 1 <= B!l4(()) in return ()"
                        + ",  {end} z3: Int, l3(w3: 1) |-> return () })"
                        + "in let w3 : 1 <= B!l1(()) in let hh : " + h2s + " <= [h ()] in suspend hh, 0");

        System.out.println(lethA);
        EALOutType out1u = (EALOutType) parseSessionType(out1us);
        lethA.type(new Gamma(), out1u);

        //---------------
        // config < A, idle, c[A] |-> let h = ... in ... >
        System.out.println();

        EAPActiveThread tA = rf.activeThread(lethA, s, A);
        LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaA = new LinkedHashMap<>();
        EAPConfig<?> cA = rf.config(p1, tA, sigmaA, pf.factory.intt(0));

        LinkedHashMap<Pair<EAPSid, Role>, EALType> env = new LinkedHashMap<>();
        env.put(new EAPPair<>(s, A), out1u);
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

        String h1s = "Handler (" + in1us + ")";
        String htsB = "{" + in1us + "} 1 ->" + h1s + "{" + recXBs + "}";
        EAPLet leth = (EAPLet) parseM(
                "let h: " + htsB + " <= return (rec f{  " + in1us + "} (w1: 1):" + h1s + "{" + recXBs + "} ."
                        + "return handler A { {" + out2mus + "} d: Int, l1(w2: 1) "
                        + " |-> let y: 1 <= A!l2(()) in let z : " + h1s + " <= [f ()] in suspend z, 42 "
                        + ", {end} d: Int, l4(w4: 1) |-> return ()"
                        + "})"
                        + "in let hh : " + h1s + " <= [h ()] in suspend hh, 0");

        System.out.println(leth);
        EALRecType recXB = (EALRecType) parseSessionType(recXBs);
        leth.type(new Gamma(), recXB);

        //--------------
        // config < B, idle, c[B] |-> let h = ... in ... } >
        System.out.println();

        EAPActiveThread tB = rf.activeThread(leth, s, B);
        LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaB = new LinkedHashMap<>();
        EAPConfig cB = rf.config(p2, tB, sigmaB, pf.factory.intt(0));

        env = new LinkedHashMap<>();
        env.put(new EAPPair<>(s, B), recXB);
        System.out.println("Typing cB: " + cB + " ,, " + env);
        cB.type(new Gamma(), new Delta(env));
        //*/

        // ----

        System.out.println("\n---");
        System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);

        LinkedHashMap<EAPPid, EAPConfig<?>> cs = new LinkedHashMap<>();
        cs.put(cA.pid, cA);
        cs.put(cB.pid, cB);

        env.put(new EAPPair<>(s, A), out1u);
        env.put(new EAPPair<>(s, B), recXB);
        System.out.println(env);
        EAPSystem sys = rf.system(lf, new Delta(env), cs);
        System.out.println(sys);
        sys.type(new Gamma(), new Delta());

        run(sys, 100);
    }

    private static void ex6(
            LTypeFactory lf, EAPFactory pf, EAPRuntimeFactory rf, EATypeFactory tf) {

        Role A = new Role("A");
        Role B = new Role("B");
        EAPSid s = rf.sid("s");
        EAPPid p1 = rf.pid("p1");
        EAPPid p2 = rf.pid("p2");

        EALOutType out1 = (EALOutType) parseSessionType("B!{l1(Bool).end}");
        EALInType in1 = (EALInType) parseSessionType("A?{l1(Bool).end}");
        EAPExpr sendAB = parseM("let x: Bool <= return 3 < 2 in if x then B!l1(x) else B!l1(x)");
        EAPActiveThread tA = rf.activeThread(sendAB, s, A);
        LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaA = new LinkedHashMap<>();
        /*LinkedHashMap<Pair<EAPSid, Role>, Integer> stateA = new LinkedHashMap<>();
        stateA.put(new EAPPair<>(s, A), 0);
        EAPConfig cA = rf.config(p1, tA, sigmaA, stateA);*/
        EAPConfig cA = rf.config(p1, tA, sigmaA, pf.factory.intt(0));

        EAPVar x = pf.var("x");
        EAPHandlers hsB = (EAPHandlers) parseV("handler A { {end} z1: Int, l1(x: Bool)  |-> return () }");
        EAPIdle idle = rf.idle();
        LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaB = new LinkedHashMap<>();
        sigmaB.put(new EAPPair<>(s, B), hsB);
        /*LinkedHashMap<Pair<EAPSid, Role>, Integer> stateB = new LinkedHashMap<>();
        stateB.put(new EAPPair<>(s, B), 42);
        EAPConfig cB = rf.config(p2, idle, sigmaB, stateB);*/
        EAPConfig cB = rf.config(p2, idle, sigmaB, pf.factory.intt(0));

        System.out.println(cA);
        System.out.println(cB);

        System.out.println("Typing eA: " + out1 + " ,, " + sendAB.type(new Gamma(), out1));

        LinkedHashMap<Pair<EAPSid, Role>, EALType> env = new LinkedHashMap<>();
        env.put(new EAPPair<>(s, A), out1);
        System.out.println("Typing cA: " + cA + " ,, " + env);
        cA.type(new Gamma(), new Delta(env));

        LinkedHashMap<EAName, EAValType> map = new LinkedHashMap<>();
        map.put(x, tf.val.unit());  // XXX FIXME
        Gamma gamma = new Gamma(map, new LinkedHashMap<>(), null, EAIntType.INT);
        System.out.println("Typing hB: " + hsB.type(gamma));

        env = new LinkedHashMap<>();
        env.put(new EAPPair<>(s, B), in1);
        System.out.println("Typing cB: " + cB + " ,, " + env);
        cB.type(new Gamma(), new Delta(env));

        LinkedHashMap<EAPPid, EAPConfig<?>> cs = new LinkedHashMap<>();
        cs.put(p1, cA);
        cs.put(p2, cB);
        //EAPSystem sys = rf.system(cs);
        env.put(new EAPPair<>(s, A), out1);
        EAPSystem sys = rf.system(lf, new Delta(env), cs);
        System.out.println(sys);
        sys.type(new Gamma(), new Delta());

        run(sys, -1);
    }

    private static void ex5(
            LTypeFactory lf, EAPFactory pf, EAPRuntimeFactory rf, EATypeFactory tf) {
        Role A = new Role("A");
        Role B = new Role("B");
        Op l1 = new Op("l1");
        Op l2 = new Op("l2");
        Op l3 = new Op("l3");
        //EAPUnit unit = pf.unit();
        EAPSid s = rf.sid("s");
        EAPPid p1 = rf.pid("p1");
        EAPPid p2 = rf.pid("p2");
        EAPVar h = pf.var("h");
        EAPVar hh = pf.var("hh");
        EAPVar x = pf.var("x");
        EAPVar y = pf.var("y");
        EAPVar z = pf.var("z");
        EAPVar zz = pf.var("zz");
        EAPVar w1 = pf.var("w1");
        EAPVar w2 = pf.var("w2");
        EAPFuncName f = new EAPFuncName("f");
        RecVar X = new RecVar("X");

        // ----

        // mu X . p&{ l2(unit) . p+{ l1(unit) . X) }, l3(unit) . end }
        LinkedHashMap<Op, EAPPair<EAValType, EALType>> cases = new LinkedHashMap<>();
        /*cases.put(l1, new EAPPair<>(tf.val.unit(), tf.local.recvar(X)));
        EALOutType out1 = tf.local.out(B, cases);
        cases = new LinkedHashMap<>();
        cases.put(l2, new EAPPair<>(tf.val.unit(), out1));
        cases.put(l3, new EAPPair<>(tf.val.unit(), tf.local.end()));
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
        cases.put(l1, new EAPPair<>(tf.val.unit(), recXA));
        EALOutType out1u = tf.local.out(B, cases);*/
        String out1us = "B!{l1(1)." + recXA + "}";
        EALOutType out1u = (EALOutType) parseSessionType(out1us);

        /*cases = new LinkedHashMap<>();
        cases.put(l2, new EAPPair<>(tf.val.unit(), out1u));
        cases.put(l3, new EAPPair<>(tf.val.unit(), tf.local.end()));
        EALInType in2u = tf.local.in(B, cases);
        EAHandlersType h2 = tf.val.handlers(in2u);*/
        String in2us = "B?{l2(1)." + out1us + ", l3(1).end}";
        EALInType in2u = (EALInType) parseSessionType(in2us);
        String h2s = "Handler(" + in2us + ")";
        EAHandlersType h2 = (EAHandlersType) parseA(h2s);

        // ----
        // let h = return rec f(_). handler B { l2(_) |-> let y = B!l1() in let z = f() in suspend z,
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
        EAFuncType ftA = (EAFuncType) parseA(ftAs);
        EAPLet lethA = (EAPLet) parseM(
                "let h : " + ftAs + " <= return rec f {" + in2us + "} (w1 :1) : " + h2s + " {" + recXAs
                        + "} . return handler B { {" + out1us + "} z2: Int, l2(w2: 1) |-> let y :1 <= B!l1(())"
                        + "in let z : " + h2s + " <= [f ()] in suspend z, 42 ,"
                        + "{end} z3: Int, l3(w2: 1) |-> return () } "
                        + "in let w1 :1 <= B!l1(()) in let hh: " + h2s + " <= [h ()] in suspend hh, 42");

        System.out.println(lethA);
        lethA.type(new Gamma(), out1u);

        //---------------
        // config < A, idle, c[A] |-> let h = ... in ... >
        System.out.println();

        EAPActiveThread tA = rf.activeThread(lethA, s, A);
        LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaA = new LinkedHashMap<>();
        /*LinkedHashMap<Pair<EAPSid, Role>, Integer> stateA = new LinkedHashMap<>();
        stateA.put(new EAPPair<>(s, A), 0);
        EAPConfig cA = rf.config(p1, tA, sigmaA, stateA);*/
        EAPConfig cA = rf.config(p1, tA, sigmaA, pf.factory.intt(0));

        LinkedHashMap<Pair<EAPSid, Role>, EALType> env = new LinkedHashMap<>();
        //env.put(new EAPPair<>(s, A), recXA);
        env.put(new EAPPair<>(s, A), out1u);
        System.out.println("Typing cA: " + cA + " ,, " + env);
        cA.type(new Gamma(), new Delta(env));

        // ----
        System.out.println();

        // !!! no branch/select subtyping
        // mu X . p&{ l1(unit) . p+{ l2(unit) . X, l3(unit).end ) } }
        /*cases = new LinkedHashMap<>();
        cases.put(l2, new EAPPair<>(tf.val.unit(), tf.local.recvar(X)));
        cases.put(l3, new EAPPair<>(tf.val.unit(), tf.local.end()));
        EALOutType out2 = tf.local.out(A, cases);
        cases = new LinkedHashMap<>();
        cases.put(l1, new EAPPair<>(tf.val.unit(), out2));
        EALInType in1 = tf.local.in(A, cases);
        EALRecType recXB = tf.local.rec(X, in1);*/
        String out2s = "A!{ l2(1) . X, l3(1).end }";
        //EALOutType out2 = (EALOutType) parseSessionType(out2s);
        String in1s = "A?{ l1(1) . " + out2s + " }";
        //EALInType in1 = (EALInType) parseSessionType(in1s);
        String recXBs = "mu X . " + in1s;
        EALRecType recXB = (EALRecType) parseSessionType(recXBs);

        /*cases = new LinkedHashMap<>();
        cases.put(l2, new EAPPair<>(tf.val.unit(), recXB));
        cases.put(l3, new EAPPair<>(tf.val.unit(), tf.local.end()));
        EALOutType out2mu = tf.local.out(A, cases);*/
        String out2mus = "A!{l2(1) . " + recXBs + ", l3(1) . end }";
        EALOutType out2mu = (EALOutType) parseSessionType(out2mus);

        // p&{ l1(unit) . p+{ l2(unit) . [mu X . p&{ l1(unit) . p+{ l2(unit) . X, l3(unit).end ) } }], l3(unit).end } }
        /*cases = new LinkedHashMap<>();
        cases.put(l2, new EAPPair<>(tf.val.unit(), recXB));
        cases.put(l3, new EAPPair<>(tf.val.unit(), tf.local.end()));
        EALOutType out2u = tf.local.out(A, cases);
        cases = new LinkedHashMap<>();
        cases.put(l1, new EAPPair<>(tf.val.unit(), out2u));
        EALInType in1u = tf.local.in(A, cases);*/
        String out2us = out2mus;
        //EALOutType out2u = (EALOutType) parseSessionType(out2us);
        String in1us = "A?{l1(1) . " + out2us + "}";
        //EALInType in1u = (EALInType) parseSessionType(in1us);

        //EAHandlersType h1 = tf.val.handlers(in1u);
        String h1s = "Handler(" + in1us + ")";
        EAHandlersType h1 = (EAHandlersType) parseA(h1s);
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
        EAPLet leth = (EAPLet) parseM(
                //"let h : {A?{l1(1).A!{l2(1).mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}, l3(1).end}}}1 -> Handler(A?{l1(1).A!{l2(1).mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}, l3(1).end}}) {mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}} <= return rec f { A?{l1(1).A!{l2(1).mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}, l3(1).end}}} (w1 :1) :Handler(A?{l1(1).A!{l2(1).mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}, l3(1).end}}) {mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}} . return handler A { l1(w2: 1) : A!{l2(1).mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}, l3(1).end} |-> let y :1 <= A!l3(()) in return () } in let hh :Handler(A?{l1(1).A!{l2(1).mu X.A?{l1(1).A!{l2(1).X, l3(1).end}}, l3(1).end}}) <= [h ()] in suspend hh");
                "let h: " + fts + " <= return rec f {" + in1us + "} (w1 :1): " + h1s + "{" + recXBs
                        + "} . return handler A { {" + out2us + "} z1: Int, l1(w2: 1) |-> let y :1 <= A!l3(()) in return () } "
                        + "in let hh: " + h1s + " <= [h ()] in suspend hh, 42");

        System.out.println(leth);
        leth.type(new Gamma(), recXB);

        //--------------
        // config < B, idle, c[B] |-> let h = ... in ... >
        System.out.println();

		/*LinkedHashMap<EAName, EAValType> map = new LinkedHashMap<>();
		map.put(x, tf.val.unit());
		Gamma gamma = new Gamma(map, new LinkedHashMap<>());
		System.out.println("Typing hB: " + hsB1.type(gamma));

		LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaB = new LinkedHashMap<>();
		sigmaB.put(new EAPPair<>(s, B), hsB1);  // !!! TODO make sigma concrete, e.g., for typing
		EAPConfig cB = rf.config(p2, rf.idle(), sigmaB);*/

        EAPActiveThread tB = rf.activeThread(leth, s, B);
        LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaB = new LinkedHashMap<>();
        /*LinkedHashMap<Pair<EAPSid, Role>, Integer> stateB = new LinkedHashMap<>();
        stateB.put(new EAPPair<>(s, B), 0);
        EAPConfig cB = rf.config(p2, tB, sigmaB, stateB);*/
        EAPConfig cB = rf.config(p2, tB, sigmaB, pf.factory.intt(0));

        env = new LinkedHashMap<>();
        env.put(new EAPPair<>(s, B), recXB);
        System.out.println("Typing cB: " + cB + " ,, " + env);
        cB.type(new Gamma(), new Delta(env));
        //*/

        // ----

        System.out.println("\n---");
        System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);

        LinkedHashMap<EAPPid, EAPConfig<?>> cs = new LinkedHashMap<>();
        cs.put(p1, cA);
        cs.put(p2, cB);

        env.put(new EAPPair<>(s, A), out1u);
        //env.put(new EAPPair<>(s, B), in1u);  // !!! cf. EAPSystem this.annots.map.get(k2) -- use unfolded as annot -- XXX that only allows that many number of unfoldings during execution
        env.put(new EAPPair<>(s, B), recXB);
        System.out.println(env);
        EAPSystem sys = rf.system(lf, new Delta(env), cs);
        System.out.println(sys);
		/*Map<EAPPid, EAPConfig> cfgs = sys.getConfigs();
		//System.out.println("Typing p1/A: " + cfgs.get(p1));
		//cfgs.get(p1).type(new Gamma(), new Delta(env));  // TODO env for p1/A
		System.out.println("Typing p2/B: " + cfgs.get(p2));
		cfgs.get(p2).type(new Gamma(), new Delta(env));*/
        //env.put(new EAPPair<>(s, A), out1);
        //System.out.println(env);
        ////sys.type(new Gamma(), new Delta(), new Delta(env));
        sys.type(new Gamma(), new Delta());

        run(sys, -1);

        /*System.out.println();
        sys = sys.reduce(p1);
        System.out.println(sys);
        env.put(new EAPPair<>(s, A), out1u);
        env.put(new EAPPair<>(s, B), recXB);
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

    private static void ex4(
            LTypeFactory lf, EAPFactory pf, EAPRuntimeFactory rf, EATypeFactory tf) {
        Role A = new Role("A");
        Role B = new Role("B");
        Op l1 = new Op("l1");
        Op l2 = new Op("l2");
        //EAPUnit unit = pf.unit();
        EAPSid s = rf.sid("s");
        EAPPid p1 = rf.pid("p1");
        EAPPid p2 = rf.pid("p2");
        EAPVar h = pf.var("h");
        EAPVar hh = pf.var("hh");
        EAPVar x = pf.var("x");
        EAPVar y = pf.var("y");
        EAPVar z = pf.var("z");
        EAPVar zz = pf.var("zz");
        EAPVar w1 = pf.var("w1");
        EAPVar w2 = pf.var("w2");
        EAPFuncName f = new EAPFuncName("f");
        RecVar X = new RecVar("X");

        // ----

        // XXX mu X . p+{ l1(unit) . p&{ l2(unit) . X) } } XXX
        // mu X . p&{ l2(unit) . p+{ l1(unit) . X } }

        LinkedHashMap<Op, EAPPair<EAValType, EALType>> cases = new LinkedHashMap<>();

        /*cases.put(l1, new EAPPair<>(tf.val.unit(), tf.local.recvar(X)));
        EALOutType out1 = tf.local.out(B, cases);
        cases = new LinkedHashMap<>();
        cases.put(l2, new EAPPair<>(tf.val.unit(), out1));
        EALInType in2 = tf.local.in(B, cases);
        EALRecType recXA = tf.local.rec(X, in2);*/

        String out1s = "B!{l1(1).X}";
        String in2s = "B?{l2(1)." + out1s + "}";
        //EALInType in2 = (EALInType) parseSessionType(in2s);
        String recXAs = "mu X." + in2s;
        //EALRecType recXA = (EALRecType) parseSessionType(recXAs);

		/*cases = new LinkedHashMap<>();
		cases.put(l2, new EAPPair<>(tf.val.unit(), recXA));
		EALInType in2mu = tf.local.in(B, cases);*/

        // XXX p+{ l1(unit) . p&{ l2(unit) . [mu X . p+{ l1(unit) . p&{ l2(unit) . X) } }] } } XXX
        // p+{ l1(unit) . [mu X . p&{ l2(unit) . p+{ l1(unit) . X) } }] }
        /*cases = new LinkedHashMap<>();
        cases.put(l1, new EAPPair<>(tf.val.unit(), recXA));
        //EALOutType out1u = tf.local.out(B, cases);*/
        String out1us = "B!{l1(1)." + recXAs + "}";
        EALOutType out1u = (EALOutType) parseSessionType(out1us);

        /*cases = new LinkedHashMap<>();
        cases.put(l2, new EAPPair<>(tf.val.unit(), out1u));
        //EALInType in2u = tf.local.in(B, cases);*/
        String in2us = "B?{l2(1)." + out1us + "}";  // unfolding of recXA
        //EALInType in2u = (EALInType) parseSessionType(in2us);
        ////EAHandlersType h2 = tf.val.handlers(in2u);
        String h2s = "Handler (" + in2us + ")";
        //EAHandlersType h2 = (EAHandlersType) parseA(h2s);

        // ----
        // let h = return rec f(_). handler B { l2(_) |-> let y = B!l1() in let z = f() in suspend z }  XXX typos
        // in [ let _ = B!l1() in let hh = h() in suspend hh ]

        // h type ... EAFuncType ftA = tf.val.func(tf.val.unit(), in2u, recXA, h2);
        String hts = "{" + in2us + "} 1-> " + h2s + "{" + recXAs + "}";
        //EAFuncType ht = (EAFuncType) parseA();
        // z type EAHandlersType h2 = tf.val.handlers(in2u);
        // ..., in2u, recXA, h2
        EAPLet lethA = (EAPLet) parseM(
                "let h: " + hts + " <= return (rec f{ " + in2us + "} (w1: 1 ):" + h2s + "{" + recXAs
                        + "} . return handler B { {" + out1us + "} z2: Int, l2(w2: 1) "
                        + " |-> let y: 1 <= B!l1(()) in let z : " + h2s + " <= [f ()] in suspend z, 42 })"
                        + "in let w3 : 1 <= B!l1(()) in let hh : " + h2s + " <= [h ()] in suspend hh, 42");

        /*//let z = f() in suspend z
        EAPSuspend suszA = pf.suspend(z);
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
        //EAPLet lethA = pf.let(h, ftA, retfA, wA);*/
        System.out.println(lethA);
        lethA.type(new Gamma(), out1u);

        //---------------
        // config < A, idle, c[A] |-> let h = ... in ... >
        System.out.println();

        EAPActiveThread tA = rf.activeThread(lethA, s, A);
        LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaA = new LinkedHashMap<>();
        /*LinkedHashMap<Pair<EAPSid, Role>, Integer> stateA = new LinkedHashMap<>();
        stateA.put(new EAPPair<>(s, A), 0);
        EAPConfig cA = rf.config(p1, tA, sigmaA, stateA);*/
        EAPConfig cA = rf.config(p1, tA, sigmaA, pf.factory.intt(0));

        LinkedHashMap<Pair<EAPSid, Role>, EALType> env = new LinkedHashMap<>();
        //env.put(new EAPPair<>(s, A), recXA);
        env.put(new EAPPair<>(s, A), out1u);
        System.out.println("Typing cA: " + cA + " ,, " + env);
        cA.type(new Gamma(), new Delta(env));


        // ----
        System.out.println();

        // mu X . p&{ l1(unit) . p+{ l2(unit) . X) } }
        /*cases = new LinkedHashMap<>();
        cases.put(l2, new EAPPair<>(tf.val.unit(), tf.local.recvar(X)));
        EALOutType out2 = tf.local.out(A, cases);
        cases = new LinkedHashMap<>();
        cases.put(l1, new EAPPair<>(tf.val.unit(), out2));
        EALInType in1 = tf.local.in(A, cases);
        EALRecType recXB = tf.local.rec(X, in1);*/
        String out2s = "A!{l2(1).X}";
        String in1s = "A?{l1(1)." + out2s + "}";
        //EALInType in1 = (EALInType) parseSessionType(in1s);
        String recXBs = "mu X." + in1s;
        EALRecType recXB = (EALRecType) parseSessionType(recXBs);

        /*cases = new LinkedHashMap<>();
        cases.put(l2, new EAPPair<>(tf.val.unit(), recXB));
        EALOutType out2mu = tf.local.out(A, cases);*/
        String out2mus = "A!{l2(1)." + recXBs + "}";
        //EALOutType out2mu = (EALOutType) parseSessionType(out2mus);

        // p&{ l1(unit) . p+{ l2(unit) . [mu X . p&{ l1(unit) . p+{ l2(unit) . X) } }] } }
        /*cases = new LinkedHashMap<>();
        cases.put(l2, new EAPPair<>(tf.val.unit(), recXB));
        EALOutType out2u = tf.local.out(A, cases);
        cases = new LinkedHashMap<>();
        cases.put(l1, new EAPPair<>(tf.val.unit(), out2u));
        EALInType in1u = tf.local.in(A, cases);*/
        String in1us = "A?{l1(1)." + out2mus + "}";
        //EALInType in1u = (EALInType) parseSessionType(in1us);

        String h1s = "Handler (" + in1us + ")";
        ////EAHandlersType h1 = tf.val.handlers(in1u);
        //EAHandlersType h1 = (EAHandlersType) parseA(h1s);
        ////EAHandlersType h1fold = tf.val.handlers(recXB);

        // ---
        // let h = return rec f(_). handler A { l1(_) |-> let y = A!l2() in let z = f() in suspend z }
        // in [ let hh = h() in suspend hh ]

        String htsB = "{" + in1us + "} 1 ->" + h1s + "{" + recXBs + "}";
        EAPLet leth = (EAPLet) parseM(
                "let h: " + htsB + " <= return (rec f{  " + in1us + "} (w1: 1):" + h1s + "{" + recXBs + "} ."
                        + "return handler A { {" + out2mus + "} z1: Int, l1(w2: 1) "
                        + " |-> let y: 1 <= A!l2(()) in let z : " + h1s + " <= [f ()] in suspend z, 42 })"
                        + "in let hh : " + h1s + " <= [h ()] in suspend hh, 42");

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
        EAPHandler hB1 = pf.handler(l1, w2, tf.val.unit(), lety, out2mu);
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
        System.out.println(lethh);

        // let h = return rec f(_). ... in let hh ...
        EAFuncType ft = tf.val.func(tf.val.unit(), in1u, recXB, h1);*/
        //EAPLet leth = pf.let(h, ft, retfB, lethh);
        System.out.println(leth);
        leth.type(new Gamma(), recXB);

        //--------------
        // config < B, idle, c[B] |-> let h = ... in ... } >
        System.out.println();

		/*LinkedHashMap<EAName, EAValType> map = new LinkedHashMap<>();
		map.put(x, tf.val.unit());
		Gamma gamma = new Gamma(map, new LinkedHashMap<>());
		System.out.println("Typing hB: " + hsB1.type(gamma));

		LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaB = new LinkedHashMap<>();
		sigmaB.put(new EAPPair<>(s, B), hsB1);  // !!! TODO make sigma concrete, e.g., for typing
		EAPConfig cB = rf.config(p2, rf.idle(), sigmaB);*/

        EAPActiveThread tB = rf.activeThread(leth, s, B);
        LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaB = new LinkedHashMap<>();
        /*LinkedHashMap<Pair<EAPSid, Role>, Integer> stateB = new LinkedHashMap<>();
        stateB.put(new EAPPair<>(s, B), 0);
        EAPConfig cB = rf.config(p2, tB, sigmaB, stateB);*/
        EAPConfig cB = rf.config(p2, tB, sigmaB, pf.factory.intt(0));

        env = new LinkedHashMap<>();
        env.put(new EAPPair<>(s, B), recXB);
        System.out.println("Typing cB: " + cB + " ,, " + env);
        cB.type(new Gamma(), new Delta(env));
        //*/

        // ----

        System.out.println("\n---");
        System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);

        LinkedHashMap<EAPPid, EAPConfig<?>> cs = new LinkedHashMap<>();
        cs.put(cA.pid, cA);
        cs.put(cB.pid, cB);

        env.put(new EAPPair<>(s, A), out1u);
        //env.put(new EAPPair<>(s, B), in1u);  // !!! cf. EAPSystem this.annots.map.get(k2) -- use unfolded as annot -- XXX that only allows that many number of unfoldings during execution
        env.put(new EAPPair<>(s, B), recXB);
        System.out.println(env);
        EAPSystem sys = rf.system(lf, new Delta(env), cs);
        System.out.println(sys);
		/*Map<EAPPid, EAPConfig> cfgs = sys.getConfigs();
		//System.out.println("Typing p1/A: " + cfgs.get(p1));
		//cfgs.get(p1).type(new Gamma(), new Delta(env));  // TODO env for p1/A
		System.out.println("Typing p2/B: " + cfgs.get(p2));
		cfgs.get(p2).type(new Gamma(), new Delta(env));*/
        //env.put(new EAPPair<>(s, A), out1);
        //System.out.println(env);
        ////sys.type(new Gamma(), new Delta(), new Delta(env));
        sys.type(new Gamma(), new Delta());

        run(sys, 100);

        /*sys = sys.reduce(p1);
        System.out.println("[1]");
        System.out.println(sys);
        env.put(new EAPPair<>(s, A), out1u);
        env.put(new EAPPair<>(s, B), recXB);
        System.out.println(env);
        //sys.type(new Gamma(), new Delta(), new Delta(env));
        sys.type(new Gamma(), new Delta());

        sys = sys.reduce(p2);
        System.out.println("[2]");
        System.out.println(sys);
        sys.type(new Gamma(), new Delta());

        sys = sys.reduce(p2);
        System.out.println("[3]");
        System.out.println(sys);
        sys.type(new Gamma(), new Delta());

        sys = sys.reduce(p2);
        System.out.println("[4]");
        System.out.println(sys);
        sys.type(new Gamma(), new Delta());

        sys = sys.reduce(p2);
        System.out.println("[5]");
        System.out.println(sys);
        sys.type(new Gamma(), new Delta());

        for (int i = 0; i < 10; i++) {
            //for (int i = 0; i < 1; i++) {

            sys = sys.reduce(p1);  // p1 send B1!l1
            System.out.println("[6]");
            System.out.println(sys);
            sys.type(new Gamma(), new Delta());

            sys = sys.reduce(p1);
            System.out.println("[7]");
            System.out.println(sys);
            sys.type(new Gamma(), new Delta());

            sys = sys.reduce(p1);
            System.out.println("[8]");
            System.out.println(sys);
            sys.type(new Gamma(), new Delta());

            sys = sys.reduce(p1);
            System.out.println("[9]");
            System.out.println(sys);
            sys.type(new Gamma(), new Delta());

            sys = sys.reduce(p1);  // p1 now idle and installed l2 handler
            System.out.println("[10]");
            System.out.println(sys);
            sys.type(new Gamma(), new Delta());

            sys = sys.reduce(p2);  // p2 send A!l2
            System.out.println("[11]");
            System.out.println(sys);
            sys.type(new Gamma(), new Delta());

            sys = sys.reduce(p2);
            System.out.println("[12]");
            System.out.println(sys);
            sys.type(new Gamma(), new Delta());

            sys = sys.reduce(p2);
            System.out.println("[13]");
            System.out.println(sys);
            sys.type(new Gamma(), new Delta());

            sys = sys.reduce(p2);
            System.out.println("[14]");
            System.out.println(sys);
            sys.type(new Gamma(), new Delta());

            sys = sys.reduce(p2);  // p2 now idle with installed l1 handler
            System.out.println("[15]");
            System.out.println(sys);
            sys.type(new Gamma(), new Delta());
            //* /
        }*/

    }

	/*private static void ex4i(
			LTypeFactory lf, EAPFactory pf, EAPRuntimeFactory rf, EATypeFactory tf) {
		Role A = new Role("A");
		Role B = new Role("B");
		Op l1 = new Op("l1");
		Op l2 = new Op("l2");
		//EAPUnit unit = pf.unit();
		EAPSid s = rf.sid("s");
		EAPPid p1 = rf.pid("p1");
		EAPPid p2 = rf.pid("p2");
		EAPVar x = pf.var("x");
		EAPVar y = pf.var("y");
		EAPVar z = pf.var("z");
		EAPVar zz = pf.var("zz");
		EAPFuncName f = new EAPFuncName("f");
		RecVar X = new RecVar("X");

		// mu X . p+{ l1(unit) . p&{ l2(unit) . X) } }
		LinkedHashMap<Op, EAPPair<EAValType, EALType>> cases = new LinkedHashMap<>();
		cases.put(l2, new EAPPair<>(tf.val.unit(), tf.local.recvar(X)));
		EALInType in2 = tf.local.in(B, cases);
		cases = new LinkedHashMap<>();
		cases.put(l1, new EAPPair<>(tf.val.unit(), in2));
		EALOutType out1 = tf.local.out(B, cases);
		EALRecType recX = tf.local.rec(X, out1);

		// p+{ l1(unit) . p&{ l2(unit) . [mu X . p+{ l1(unit) . p&{ l2(unit) . X) } }] } }
		cases = new LinkedHashMap<>();
		cases.put(l2, new EAPPair<>(tf.val.unit(), recX));
		EALInType inu = tf.local.in(B, cases);
		cases = new LinkedHashMap<>();
		cases.put(l1, new EAPPair<>(tf.val.unit(), inu));
		EALOutType unfoldX = tf.local.out(B, cases);

		// let x = return rec f(_). [ let y = B!l1() in suspend handler B { l2(_) |-> let zz = f() in suspend zz } ]
		// in x()
		EAPSend sendAB1 = pf.send(B, l1, pf.unit());
		LinkedHashMap<Op, EATriple<EAPVar, EAValType, EAPExpr>> Hs = new LinkedHashMap<>();
		EAPApp appx = pf.app(f, pf.unit());

		EAPSuspend sus2 = pf.suspend(zz);
		EAPLet let2 = pf.let(zz, ...handlerintype..., appx, sus2);  // HERE fix "infer" for suspend h

		Hs.put(l2, new EATriple<>(z, tf.val.unit(), let2));
		EAPHandlers hA = pf.handlers(B, Hs);
		EAPSuspend sushA = pf.suspend(hA);
		EAPLet lety = pf.let(y, tf.val.unit(), sendAB1, sushA);
		EAValType typeB = ...handlerintype...;  // cf. T-SuspendSync, (A, S') can be anything -- needs to match type of zz for the rec
		EAPRec recf = pf.rec(f, z, tf.val.unit(), lety, unfoldX, recX, typeB);
		EAPReturn retf = pf.returnn(recf);
		EAPLet letx = pf.let(x, tf.val.unit(), retf, appx);

		System.out.println("Typing letx: " + letx + " ,, " + letx.type(new Gamma(), unfoldX));

		/ *
		EAPActiveThread tA = rf.activeThread(let, s, A);
		LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaA = new LinkedHashMap<>();
		EAPConfig cA = rf.config(p1, tA, sigmaA);

		LinkedHashMap<Pair<EAPSid, Role>, EALType> env = new LinkedHashMap<>();
		env.put(new EAPPair<>(s, A), out1);
		System.out.println("Typing cA: " + cA + " ,, " + env);
		cA.type(new Gamma(), new Delta(env));

		// ----

		// idle, s[B] |-> handler B { l1(x) -> suspend(l2(x) -> return () ) }
		LinkedHashMap<Op, EATriple<EAPVar, EAValType, EAPExpr>> Hs = new LinkedHashMap<>();
		EAPIdle idle = rf.idle();
		EAPReturn ret = pf.returnn(pf.unit());
		Hs.put(l2, new EATriple<>(x, tf.val.unit(), ret));
		EAPHandlers hB2 = pf.handlers(B, Hs);

		Hs = new LinkedHashMap<>();
		EAPSuspend sus = pf.suspend(hB2);  // XXX suspend
		// l1 -> (x, continuation)
		Hs.put(l1, new EATriple<>(x, tf.val.unit(), sus));
		EAPHandlers hB1 = pf.handlers(B, Hs);

		LinkedHashMap<EAName, EAValType> map = new LinkedHashMap<>();
		map.put(x, tf.val.unit());
		Gamma gamma = new Gamma(map);
		System.out.println("Typing hB: " + hB1.type(gamma));

		LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaB = new LinkedHashMap<>();
		sigmaB.put(new EAPPair<>(s, B), hB1);  // !!! TODO make sigma concrete, e.g., for typing
		EAPConfig cB = rf.config(p2, idle, sigmaB);

		cases = new LinkedHashMap<>();
		cases.put(l2, new EAPPair<>(tf.val.unit(), tf.local.end()));
		EALInType in2 = tf.local.in(B, cases);
		cases = new LinkedHashMap<>();
		cases.put(l1, new EAPPair<>(tf.val.unit(), in2));
		EALInType in1 = tf.local.in(B, cases);

		env = new LinkedHashMap<>();
		env.put(new EAPPair<>(s, B), in1);
		System.out.println("Typing cB: " + cB + " ,, " + env);
		cB.type(new Gamma(), new Delta(env));

		// ----

		System.out.println("\n---");
		System.out.println("cA = " + cA);
		System.out.println("cB = " + cB);

		LinkedHashMap<EAPPid, EAPConfig> cs = new LinkedHashMap<>();
		cs.put(p1, cA);
		cs.put(p2, cB);

		env.put(new EAPPair<>(s, A), out1);
		System.out.println(env);
		EAPSystem sys = rf.system(lf, new Delta(env), cs);
		System.out.println(sys);
		/*Map<EAPPid, EAPConfig> cfgs = sys.getConfigs();
		//System.out.println("Typing p1/A: " + cfgs.get(p1));
		//cfgs.get(p1).type(new Gamma(), new Delta(env));  // TODO env for p1/A
		System.out.println("Typing p2/B: " + cfgs.get(p2));
		cfgs.get(p2).type(new Gamma(), new Delta(env));* /
		//env.put(new EAPPair<>(s, A), out1);
		//System.out.println(env);
		////sys.type(new Gamma(), new Delta(), new Delta(env));
		sys.type(new Gamma(), new Delta());

		System.out.println();
		sys = sys.reduce(p1);
		System.out.println(sys);
		env.put(new EAPPair<>(s, A), out2);
		env.put(new EAPPair<>(s, B), in2);
		System.out.println(env);
		//sys.type(new Gamma(), new Delta(), new Delta(env));
		sys.type(new Gamma(), new Delta());

		sys = sys.reduce(p1);
		System.out.println();
		System.out.println(sys);
		sys.type(new Gamma(), new Delta());

		sys = sys.reduce(p2);
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

		sys = sys.reduce(p2);
		System.out.println();
		System.out.println(sys);
		sys.type(new Gamma(), new Delta());
		* /
	}
	*/

    @Deprecated
    private static void ex3(
            LTypeFactory lf, EAPFactory pf, EAPRuntimeFactory rf, EATypeFactory tf) {
        Role A = new Role("A");
        Role B = new Role("B");
        Op l1 = new Op("l1");
        //EAPUnit unit = pf.unit();
        EAPSid s = rf.sid("s");
        EAPPid p1 = rf.pid("p1");
        EAPPid p2 = rf.pid("p2");
        EAPVar x = pf.var("x");

        // A: mu X . B!l1(unit) . X
        EAPSend sendAB = pf.send(B, l1, pf.unit());

		/*EAPSuspend sus = pf.suspend()  // HERE: recursion example -- or need to suspend an output handler?

		pf.let(x, tf.val.unit(), sendAB, )*/

		/*EAPActiveThread tA = rf.activeThread(sendAB, s, A);
		LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaA = new LinkedHashMap<>();
		EAPConfig cA = rf.config(p1, tA, sigmaA);

		// idle, s[B] |-> handler B { l1(x) |-> return(unit) }  -- l1 |-> (x, return(unit)
		LinkedHashMap<Op, EATriple<EAPVar, EAValType, EAPExpr>> Hs = new LinkedHashMap<>();
		EAPReturn ret = pf.returnn(pf.unit());
		Hs.put(l1, new EATriple<>(x, tf.val.unit(), ret));
		EAPHandlers hB = pf.handlers(B, Hs);
		EAPIdle idle = rf.idle();
		LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaB = new LinkedHashMap<>();
		sigmaB.put(new EAPPair<>(s, B), hB);
		EAPConfig cB = rf.config(p2, idle, sigmaB);

		System.out.println(cA);
		System.out.println(cB);

		LinkedHashMap<Op, EAPPair<EAValType, EALType>> cases = new LinkedHashMap<>();
		cases.put(l1, new EAPPair<>(tf.val.unit(), tf.local.end()));
		EALOutType out1 = tf.local.out(B, cases);

		System.out.println("Typing eA: " + out1 + " ,, " + sendAB.type(new Gamma(), out1));

		LinkedHashMap<Pair<EAPSid, Role>, EALType> env = new LinkedHashMap<>();
		env.put(new EAPPair<>(s, A), out1);
		System.out.println("Typing cA: " + cA + " ,, " + env);
		cA.type(new Gamma(), new Delta(env));

		cases = new LinkedHashMap<>();
		cases.put(l1, new EAPPair<>(tf.val.unit(), tf.local.end()));
		EALInType in1 = tf.local.in(B, cases);

		LinkedHashMap<EAName, EAValType> map = new LinkedHashMap<>();
		map.put(x, tf.val.unit());
		Gamma gamma = new Gamma(map);
		System.out.println("Typing hB: " + hB.type(gamma));

		env = new LinkedHashMap<>();
		env.put(new EAPPair<>(s, B), in1);
		System.out.println("Typing cB: " + cB + " ,, " + env);
		cB.type(new Gamma(), new Delta(env));

		LinkedHashMap<EAPPid, EAPConfig> cs = new LinkedHashMap<>();
		cs.put(p1, cA);
		cs.put(p2, cB);
		//EAPSystem sys = rf.system(cs);
		env.put(new EAPPair<>(s, A), out1);
		EAPSystem sys = rf.system(lf, new Delta(env), cs);
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

		sys = sys.reduce(p2);
		System.out.println();
		System.out.println(sys);
		sys.type(new Gamma(), new Delta());

		/*sys = sys.reduce(p2);
		System.out.println();
		System.out.println(sys);* /
		*/
    }

    //*
    private static void ex2(
            LTypeFactory lf, EAPFactory pf, EAPRuntimeFactory rf, EATypeFactory tf) {

        Role A = new Role("A");
        Role B = new Role("B");
        EAPSid s = rf.sid("s");
        EAPPid p1 = rf.pid("p1");
        EAPPid p2 = rf.pid("p2");
        EAPVar x = pf.var("x");
		/*Op l1 = new Op("l1");
		Op l2 = new Op("l2");
		EAPUnit unit = pf.unit();*/

		/*LinkedHashMap<Op, EAPPair<EAValType, EALType>> cases = new LinkedHashMap<>();
		cases.put(l2, new EAPPair<>(tf.val.unit(), tf.local.end()));
		EALOutType out2 = tf.local.out(B, cases);
		cases = new LinkedHashMap<>();
		cases.put(l1, new EAPPair<>(tf.val.unit(), out2));
		EALOutType out1 = tf.local.out(B, cases);*/
        EALOutType out1 = (EALOutType) parseSessionType("B!{l1(1).B!{l2(1).end}}");

		/*cases = new LinkedHashMap<>();
		cases.put(l2, new EAPPair<>(tf.val.unit(), tf.local.end()));
		EALInType in2 = tf.local.in(A, cases);
		cases = new LinkedHashMap<>();
		cases.put(l1, new EAPPair<>(tf.val.unit(), in2));
		EALInType in1 = tf.local.in(A, cases);*/
        EALInType in1 = (EALInType) parseSessionType("A?{l1(1).A?{l2(1).end}}");

        // ---

        // let x = B!l1() in B!l2()
		/*EAPSend sendAB1 = pf.send(B, l1, unit);
		EAPSend sendAB2 = pf.send(B, l2, unit);

		EAPLet let = pf.let(x, tf.val.unit(), sendAB1, sendAB2);*/
        EAPLet let = (EAPLet) parseM("let x: 1 <= B!l1(()) in B!l2(())");

        System.out.println("Typing eA: " + out1 + " ,, " + let.type(new Gamma(), out1));

        EAPActiveThread tA = rf.activeThread(let, s, A);
        LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaA = new LinkedHashMap<>();
        /*LinkedHashMap<Pair<EAPSid, Role>, Integer> stateA = new LinkedHashMap<>();
        stateA.put(new EAPPair<>(s, A), 0);
        EAPConfig cA = rf.config(p1, tA, sigmaA, stateA);*/
        EAPConfig cA = rf.config(p1, tA, sigmaA, pf.factory.intt(0));


        LinkedHashMap<Pair<EAPSid, Role>, EALType> env = new LinkedHashMap<>();
        env.put(new EAPPair<>(s, A), out1);
        System.out.println("Typing cA: " + cA + " ,, " + env);
        cA.type(new Gamma(), new Delta(env));

        // ----

        EAPIdle idle = rf.idle();
        // idle, s[B] |-> handler A { l1(x) -> suspend(l2(x) -> return () ) }
		/*LinkedHashMap<Op, EAPHandler> Hs = new LinkedHashMap<>();
		EAPReturn ret = pf.returnn(pf.unit());
		EAPHandler hB2 = pf.handler(l2, x, tf.val.unit(), ret, tf.local.end());
		Hs.put(l2, hB2);
		EAPHandlers hsB2 = pf.handlers(A, Hs);

		Hs = new LinkedHashMap<>();
		EAPSuspend sus = pf.suspend(hsB2);  // XXX suspend
		// l1 -> (x, continuation)
		EAPHandler hB1 = pf.handler(l1, x, tf.val.unit(), sus, in2);
		Hs.put(l1, hB1);
		EAPHandlers hsB1 = pf.handlers(A, Hs);*/
        EAPHandlers hsB1 = (EAPHandlers) parseV(
                "handler A { {A?{l2(1).end}} z:Int, l1(x: 1)  |->"
                        + "suspend (handler A { {end} z:Int, l2(x: 1) |-> return () }), 42 }");

        LinkedHashMap<EAName, EAValType> map = new LinkedHashMap<>();
        map.put(x, tf.val.unit());
        Gamma gamma = new Gamma(map, new LinkedHashMap<>(), null, EAIntType.INT);
        System.out.println("1111111: " + hsB1 + " ,, " + gamma);
        System.out.println("Typing hB: " + hsB1.type(gamma));

        LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaB = new LinkedHashMap<>();
        sigmaB.put(new EAPPair<>(s, B), hsB1);  // !!! TODO make sigma concrete, e.g., for typing
        /*LinkedHashMap<Pair<EAPSid, Role>, Integer> stateB = new LinkedHashMap<>();
        stateB.put(new EAPPair<>(s, B), 0);
        EAPConfig cB = rf.config(p2, idle, sigmaB, stateB);*/
        EAPConfig cB = rf.config(p2, idle, sigmaB, pf.factory.intt(0));

        env = new LinkedHashMap<>();
        env.put(new EAPPair<>(s, B), in1);
        System.out.println("Typing cB: " + cB + " ,, " + env);
        cB.type(new Gamma(), new Delta(env));

        // ----

        System.out.println("\n---");
        System.out.println("cA = " + cA);
        System.out.println("cB = " + cB);

        LinkedHashMap<EAPPid, EAPConfig<?>> cs = new LinkedHashMap<>();
        cs.put(p1, cA);
        cs.put(p2, cB);

        env.put(new EAPPair<>(s, A), out1);
        env.put(new EAPPair<>(s, B), in1);
        System.out.println(env);
        EAPSystem sys = rf.system(lf, new Delta(env), cs);
        System.out.println(sys);
		/*Map<EAPPid, EAPConfig> cfgs = sys.getConfigs();
		//System.out.println("Typing p1/A: " + cfgs.get(p1));
		//cfgs.get(p1).type(new Gamma(), new Delta(env));  // TODO env for p1/A
		System.out.println("Typing p2/B: " + cfgs.get(p2));
		cfgs.get(p2).type(new Gamma(), new Delta(env));*/
        //env.put(new EAPPair<>(s, A), out1);
        //System.out.println(env);
        ////sys.type(new Gamma(), new Delta(), new Delta(env));
        sys.type(new Gamma(), new Delta());

        run(sys, -1);
		/*
		System.out.println();
		sys = sys.reduce(p1);
		System.out.println(sys);
		/*env.put(new EAPPair<>(s, A), out2);
		env.put(new EAPPair<>(s, B), in2);
		System.out.println(env);
		//sys.type(new Gamma(), new Delta(), new Delta(env));* /
		sys.type(new Gamma(), new Delta());

		sys = sys.reduce(p1);
		System.out.println();
		System.out.println(sys);
		sys.type(new Gamma(), new Delta());

		sys = sys.reduce(p2);
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

		sys = sys.reduce(p2);
		System.out.println();
		System.out.println(sys);
		sys.type(new Gamma(), new Delta());
		 */
    }
    //*/

    //*
    private static void ex1(
            LTypeFactory lf, EAPFactory pf, EAPRuntimeFactory rf, EATypeFactory tf) {

        Role A = new Role("A");
        Role B = new Role("B");
        EAPSid s = rf.sid("s");
        EAPPid p1 = rf.pid("p1");
        EAPPid p2 = rf.pid("p2");
		/*Op l1 = new Op("l1");
		EAPUnit unit = pf.unit();*/

		/*LinkedHashMap<Op, EAPPair<EAValType, EALType>> cases = new LinkedHashMap<>();
		cases.put(l1, new EAPPair<>(tf.val.unit(), tf.local.end()));
		EALOutType out1 = tf.local.out(B, cases);*/
        EALOutType out1 = (EALOutType) parseSessionType("B!{l1(Int).end}");

		/*cases = new LinkedHashMap<>();
		cases.put(l1, new EAPPair<>(tf.val.unit(), tf.local.end()));
		EALInType in1 = tf.local.in(B, cases);*/
        EALInType in1 = (EALInType) parseSessionType("A?{l1(Int).end}");

        // A: B!l1(unit)
        //EAPSend sendAB = pf.send(B, l1, unit);
        //EAPSend sendAB = (EAPSend) parseM("B!l1(42)");
        EAPLet sendAB = (EAPLet) parseM("let x: Int <= return 41 + 1 in B!l1(x)");
        EAPActiveThread tA = rf.activeThread(sendAB, s, A);
        LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaA = new LinkedHashMap<>();
        /*LinkedHashMap<Pair<EAPSid, Role>, Integer> stateA = new LinkedHashMap<>();
        stateA.put(new EAPPair<>(s, A), 0);
        EAPConfig cA = rf.config(p1, tA, sigmaA, stateA);*/
        EAPConfig cA = rf.config(p1, tA, sigmaA, pf.factory.intt(0));

        EAPVar x = pf.var("x");
        // idle, s[B] |-> handler B { l1(x) |-> return(unit) }  -- l1 |-> (x, return(unit)
		/*LinkedHashMap<Op, EAPHandler> Hs = new LinkedHashMap<>();
		EAPReturn ret = pf.returnn(unit);
		EAPHandler hB = pf.handler(l1, x, tf.val.unit(), ret, tf.local.end());
		Hs.put(l1, hB);
		EAPHandlers hsB = pf.handlers(B, Hs);*/
        EAPHandlers hsB = (EAPHandlers) parseV("handler A { {end} z: Int, l1(x: Int)  |-> return () }");
        EAPIdle idle = rf.idle();
        LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaB = new LinkedHashMap<>();
        sigmaB.put(new EAPPair<>(s, B), hsB);
        /*LinkedHashMap<Pair<EAPSid, Role>, Integer> stateB = new LinkedHashMap<>();
        stateB.put(new EAPPair<>(s, B), 0);
        EAPConfig cB = rf.config(p2, idle, sigmaB, stateB);*/
        EAPConfig cB = rf.config(p2, idle, sigmaB, pf.factory.intt(0));

        System.out.println(cA);
        System.out.println(cB);

        System.out.println("Typing eA: " + out1 + " ,, " + sendAB.type(new Gamma(), out1));

        LinkedHashMap<Pair<EAPSid, Role>, EALType> env = new LinkedHashMap<>();
        env.put(new EAPPair<>(s, A), out1);
        System.out.println("Typing cA: " + cA + " ,, " + env);
        cA.type(new Gamma(), new Delta(env));

        LinkedHashMap<EAName, EAValType> map = new LinkedHashMap<>();
        map.put(x, tf.val.unit());
        Gamma gamma = new Gamma(map, new LinkedHashMap<>(), null, EAIntType.INT);
        System.out.println("Typing hB: " + hsB.type(gamma));

        env = new LinkedHashMap<>();
        env.put(new EAPPair<>(s, B), in1);
        System.out.println("Typing cB: " + cB + " ,, " + env);
        cB.type(new Gamma(), new Delta(env));

        LinkedHashMap<EAPPid, EAPConfig<?>> cs = new LinkedHashMap<>();
        cs.put(p1, cA);
        cs.put(p2, cB);
        //EAPSystem sys = rf.system(cs);
        env.put(new EAPPair<>(s, A), out1);
        EAPSystem sys = rf.system(lf, new Delta(env), cs);
        System.out.println(sys);
        sys.type(new Gamma(), new Delta());

        run(sys, -1);
    }
    //*/

    // steps -1 for unbounded
    static void run(EAPSystem sys, int steps) {
        int rem = steps;
        Map<EAPPid, Set<EAPPid>> pids = sys.canStep();
        for (; !pids.isEmpty() && rem != 0; rem--) {
            sys = sys.reduce(pids.keySet().iterator().next());  // FIXME HERE HERE always first act  // keyset is can-step-pids, (currently unused) Set is "partners"
            System.out.println();
            System.out.println(sys);
            sys.type(new Gamma(), new Delta());
            pids = sys.canStep();
        }

        if (steps == -1) {
            if (sys.configs.values().stream().anyMatch(x -> !x.T.isIdle() || !x.sigma.isEmpty())) {
                throw new RuntimeException("Stuck: " + sys);
            }
        }
    }

		/*
		sys = sys.reduce(p1);
		System.out.println();
		System.out.println(sys);
		sys.type(new Gamma(), new Delta());

		sys = sys.reduce(p1);
		System.out.println();
		System.out.println(sys);
		sys.type(new Gamma(), new Delta());

		sys = sys.reduce(p2);
		System.out.println();
		System.out.println(sys);
		sys.type(new Gamma(), new Delta());
		 */

		/*sys = sys.reduce(p2);
		System.out.println();
		System.out.println(sys);* /
	}
	*/


}
