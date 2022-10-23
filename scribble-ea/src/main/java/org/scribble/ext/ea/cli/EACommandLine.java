package org.scribble.ext.ea.cli;

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
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.util.EAPPair;
import org.scribble.ext.ea.util.EATriple;
import org.scribble.util.AntlrSourceException;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;



//- key point: only relevant handlers available at any one time -- cf. prev EDP works
// HERE
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



// Includes assrt-core functionality (all extra args are currently for assrt-core)
public class EACommandLine extends CommandLine
{
	public EACommandLine(String... args)
	{
		super(args);
	}

	public static void main(String[] args)
			throws CommandLineException, AntlrSourceException
	{
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

		//CommandLine.main(args);  // No main module

		LTypeFactoryImpl lf = new LTypeFactoryImpl();

		EAPFactory pf = EAPFactory.factory;
		EAPRuntimeFactory rf = EAPRuntimeFactory.factory;
		EATypeFactory tf = EATypeFactory.factory;

		//ex1(lf, pf, rf, tf);
		//ex2(lf, pf, rf, tf);
		ex4(lf, pf, rf, tf);

		//new EACommandLine(args).run();
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

		/*
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
		*/
	}

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

	private static void ex2(
			LTypeFactory lf, EAPFactory pf, EAPRuntimeFactory rf, EATypeFactory tf) {
		Role A = new Role("A");
		Role B = new Role("B");
		Op l1 = new Op("l1");
		Op l2 = new Op("l2");
		EAPUnit unit = pf.unit();
		EAPSid s = rf.sid("s");
		EAPPid p1 = rf.pid("p1");
		EAPPid p2 = rf.pid("p2");
		EAPVar x = pf.var("x");

		// let x = B!l1() in B!l2()
		EAPSend sendAB1 = pf.send(B, l1, unit);
		EAPSend sendAB2 = pf.send(B, l2, unit);

		EAPLet let = pf.let(x, tf.val.unit(), sendAB1, sendAB2);

		LinkedHashMap<Op, EAPPair<EAValType, EALType>> cases = new LinkedHashMap<>();
		cases.put(l2, new EAPPair<>(tf.val.unit(), tf.local.end()));
		EALOutType out2 = tf.local.out(B, cases);
		cases = new LinkedHashMap<>();
		cases.put(l1, new EAPPair<>(tf.val.unit(), out2));
		EALOutType out1 = tf.local.out(B, cases);

		System.out.println("Typing eA: " + out1 + " ,, " + let.type(new Gamma(), out1));

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
		Gamma gamma = new Gamma(map, new LinkedHashMap<>());
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
		cfgs.get(p2).type(new Gamma(), new Delta(env));*/
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
	}

	private static void ex1(
			LTypeFactory lf, EAPFactory pf, EAPRuntimeFactory rf, EATypeFactory tf) {
		Role A = new Role("A");
		Role B = new Role("B");
		Op l1 = new Op("l1");
		EAPUnit unit = pf.unit();
		EAPSid s = rf.sid("s");
		EAPPid p1 = rf.pid("p1");
		EAPPid p2 = rf.pid("p2");

		// A: B!l1(unit)
		EAPSend sendAB = pf.send(B, l1, unit);
		EAPActiveThread tA = rf.activeThread(sendAB, s, A);
		LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaA = new LinkedHashMap<>();
		EAPConfig cA = rf.config(p1, tA, sigmaA);

		// idle, s[B] |-> handler B { l1(x) |-> return(unit) }  -- l1 |-> (x, return(unit)
		LinkedHashMap<Op, EATriple<EAPVar, EAValType, EAPExpr>> Hs = new LinkedHashMap<>();
		EAPVar x = pf.var("x");
		EAPReturn ret = pf.returnn(unit);
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
		Gamma gamma = new Gamma(map, new LinkedHashMap<>());
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
		System.out.println(sys);*/
	}
	//*/




}
