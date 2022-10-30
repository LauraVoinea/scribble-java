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
import org.scribble.ext.ea.core.type.value.EAFuncType;
import org.scribble.ext.ea.core.type.value.EAHandlersType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.util.EAPPair;
import org.scribble.ext.ea.util.EATriple;
import org.scribble.util.AntlrSourceException;
import org.scribble.util.Pair;

import java.util.Collections;
import java.util.LinkedHashMap;



//- key point: only relevant handlers available at any one time -- cf. prev EDP works
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
		// mu X . p&{ l2(unit) . p+{ l1(unit) . X) } }
		LinkedHashMap<Op, EAPPair<EAValType, EALType>> cases = new LinkedHashMap<>();
		cases.put(l1, new EAPPair<>(tf.val.unit(), tf.local.recvar(X)));
		EALOutType out1 = tf.local.out(B, cases);
		cases = new LinkedHashMap<>();
		cases.put(l2, new EAPPair<>(tf.val.unit(), out1));
		EALInType in2 = tf.local.in(B, cases);
		EALRecType recXA = tf.local.rec(X, in2);

		/*cases = new LinkedHashMap<>();
		cases.put(l2, new EAPPair<>(tf.val.unit(), recXA));
		EALInType in2mu = tf.local.in(B, cases);*/

		// XXX p+{ l1(unit) . p&{ l2(unit) . [mu X . p+{ l1(unit) . p&{ l2(unit) . X) } }] } } XXX
		// p+{ l1(unit) . [mu X . p&{ l2(unit) . p+{ l1(unit) . X) } }] } }
		cases = new LinkedHashMap<>();
		cases.put(l1, new EAPPair<>(tf.val.unit(), recXA));
		EALOutType out1u = tf.local.out(B, cases);

		cases = new LinkedHashMap<>();
		cases.put(l2, new EAPPair<>(tf.val.unit(), out1u));
		EALInType in2u = tf.local.in(B, cases);
		EAHandlersType h2 = tf.val.handlers(in2u);

		// ----
		// let h = return rec f(_). handler B { l2(_) |-> let y = B!l1() in let z = f() in suspend z }
		// in [ let _ = B!l1() in let hh = h() in suspend hh ]

		//let z = f() in suspend z
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
		EAPLet lethA = pf.let(h, ftA, retfA, wA);
		System.out.println(lethA);
		lethA.type(new Gamma(), out1u);

		//---------------
		// config < A, idle, c[A] |-> let h = ... in ... >
		System.out.println();

		EAPActiveThread tA = rf.activeThread(lethA, s, A);
		LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaA = new LinkedHashMap<>();
		EAPConfig cA = rf.config(p1, tA, sigmaA);

		LinkedHashMap<Pair<EAPSid, Role>, EALType> env = new LinkedHashMap<>();
		//env.put(new EAPPair<>(s, A), recXA);
		env.put(new EAPPair<>(s, A), out1u);
		System.out.println("Typing cA: " + cA + " ,, " + env);
		cA.type(new Gamma(), new Delta(env));

		// ----
		System.out.println();

		// mu X . p&{ l1(unit) . p+{ l2(unit) . X) } }
		cases = new LinkedHashMap<>();
		cases.put(l2, new EAPPair<>(tf.val.unit(), tf.local.recvar(X)));
		EALOutType out2 = tf.local.out(A, cases);
		cases = new LinkedHashMap<>();
		cases.put(l1, new EAPPair<>(tf.val.unit(), out2));
		EALInType in1 = tf.local.in(A, cases);
		EALRecType recXB = tf.local.rec(X, in1);

		cases = new LinkedHashMap<>();
		cases.put(l2, new EAPPair<>(tf.val.unit(), recXB));
		EALOutType out2mu = tf.local.out(A, cases);

		// p&{ l1(unit) . p+{ l2(unit) . [mu X . p&{ l1(unit) . p+{ l2(unit) . X) } }] } }
		cases = new LinkedHashMap<>();
		cases.put(l2, new EAPPair<>(tf.val.unit(), recXB));
		EALOutType out2u = tf.local.out(A, cases);
		cases = new LinkedHashMap<>();
		cases.put(l1, new EAPPair<>(tf.val.unit(), out2u));
		EALInType in1u = tf.local.in(A, cases);

		EAHandlersType h1 = tf.val.handlers(in1u);
		//EAHandlersType h1fold = tf.val.handlers(recXB);

		// ---
		// let h = return rec f(_). handler A { l1(_) |-> let y = A!l2() in let z = f() in suspend z }
		// in [ let hh = h() in suspend hh ]

		//let z = f() in suspend z
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
		EAFuncType ft = tf.val.func(tf.val.unit(), in1u, recXB, h1);
		EAPLet leth = pf.let(h, ft, retfB, lethh);
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
		EAPConfig cB = rf.config(p1, tB, sigmaB);

		env = new LinkedHashMap<>();
		env.put(new EAPPair<>(s, B), recXB);
		System.out.println("Typing cB: " + cB + " ,, " + env);
		cB.type(new Gamma(), new Delta(env));
		//*/

		// ----

		/*
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
		Op l1 = new Op("l1");
		Op l2 = new Op("l2");
		EAPUnit unit = pf.unit();
		EAPSid s = rf.sid("s");
		EAPPid p1 = rf.pid("p1");
		EAPPid p2 = rf.pid("p2");
		EAPVar x = pf.var("x");

		LinkedHashMap<Op, EAPPair<EAValType, EALType>> cases = new LinkedHashMap<>();
		cases.put(l2, new EAPPair<>(tf.val.unit(), tf.local.end()));
		EALOutType out2 = tf.local.out(B, cases);
		cases = new LinkedHashMap<>();
		cases.put(l1, new EAPPair<>(tf.val.unit(), out2));
		EALOutType out1 = tf.local.out(B, cases);

		cases = new LinkedHashMap<>();
		cases.put(l2, new EAPPair<>(tf.val.unit(), tf.local.end()));
		EALInType in2 = tf.local.in(A, cases);
		cases = new LinkedHashMap<>();
		cases.put(l1, new EAPPair<>(tf.val.unit(), in2));
		EALInType in1 = tf.local.in(A, cases);

		// ---

		// let x = B!l1() in B!l2()
		EAPSend sendAB1 = pf.send(B, l1, unit);
		EAPSend sendAB2 = pf.send(B, l2, unit);

		EAPLet let = pf.let(x, tf.val.unit(), sendAB1, sendAB2);

		System.out.println("Typing eA: " + out1 + " ,, " + let.type(new Gamma(), out1));

		EAPActiveThread tA = rf.activeThread(let, s, A);
		LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaA = new LinkedHashMap<>();
		EAPConfig cA = rf.config(p1, tA, sigmaA);

		LinkedHashMap<Pair<EAPSid, Role>, EALType> env = new LinkedHashMap<>();
		env.put(new EAPPair<>(s, A), out1);
		System.out.println("Typing cA: " + cA + " ,, " + env);
		cA.type(new Gamma(), new Delta(env));

		// ----

		// idle, s[B] |-> handler A { l1(x) -> suspend(l2(x) -> return () ) }
		LinkedHashMap<Op, EAPHandler> Hs = new LinkedHashMap<>();
		EAPIdle idle = rf.idle();
		EAPReturn ret = pf.returnn(pf.unit());
		EAPHandler hB2 = pf.handler(l2, x, tf.val.unit(), ret, tf.local.end());
		Hs.put(l2, hB2);
		EAPHandlers hsB2 = pf.handlers(A, Hs);

		Hs = new LinkedHashMap<>();
		EAPSuspend sus = pf.suspend(hsB2);  // XXX suspend
		// l1 -> (x, continuation)
		EAPHandler hB1 = pf.handler(l1, x, tf.val.unit(), sus, in2);
		Hs.put(l1, hB1);
		EAPHandlers hsB1 = pf.handlers(A, Hs);

		LinkedHashMap<EAName, EAValType> map = new LinkedHashMap<>();
		map.put(x, tf.val.unit());
		Gamma gamma = new Gamma(map, new LinkedHashMap<>());
		System.out.println("Typing hB: " + hsB1.type(gamma));

		LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaB = new LinkedHashMap<>();
		sigmaB.put(new EAPPair<>(s, B), hsB1);  // !!! TODO make sigma concrete, e.g., for typing
		EAPConfig cB = rf.config(p2, idle, sigmaB);

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
	//*/

	private static void ex1(
			LTypeFactory lf, EAPFactory pf, EAPRuntimeFactory rf, EATypeFactory tf) {
		Role A = new Role("A");
		Role B = new Role("B");
		Op l1 = new Op("l1");
		EAPUnit unit = pf.unit();
		EAPSid s = rf.sid("s");
		EAPPid p1 = rf.pid("p1");
		EAPPid p2 = rf.pid("p2");

		LinkedHashMap<Op, EAPPair<EAValType, EALType>> cases = new LinkedHashMap<>();
		cases.put(l1, new EAPPair<>(tf.val.unit(), tf.local.end()));
		EALOutType out1 = tf.local.out(B, cases);

		cases = new LinkedHashMap<>();
		cases.put(l1, new EAPPair<>(tf.val.unit(), tf.local.end()));
		EALInType in1 = tf.local.in(B, cases);

		// A: B!l1(unit)
		EAPSend sendAB = pf.send(B, l1, unit);
		EAPActiveThread tA = rf.activeThread(sendAB, s, A);
		LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaA = new LinkedHashMap<>();
		EAPConfig cA = rf.config(p1, tA, sigmaA);

		// idle, s[B] |-> handler B { l1(x) |-> return(unit) }  -- l1 |-> (x, return(unit)
		LinkedHashMap<Op, EAPHandler> Hs = new LinkedHashMap<>();
		EAPVar x = pf.var("x");
		EAPReturn ret = pf.returnn(unit);
		EAPHandler hB = pf.handler(l1, x, tf.val.unit(), ret, tf.local.end());
		Hs.put(l1, hB);
		EAPHandlers hsB = pf.handlers(B, Hs);
		EAPIdle idle = rf.idle();
		LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaB = new LinkedHashMap<>();
		sigmaB.put(new EAPPair<>(s, B), hsB);
		EAPConfig cB = rf.config(p2, idle, sigmaB);

		System.out.println(cA);
		System.out.println(cB);

		System.out.println("Typing eA: " + out1 + " ,, " + sendAB.type(new Gamma(), out1));

		LinkedHashMap<Pair<EAPSid, Role>, EALType> env = new LinkedHashMap<>();
		env.put(new EAPPair<>(s, A), out1);
		System.out.println("Typing cA: " + cA + " ,, " + env);
		cA.type(new Gamma(), new Delta(env));

		LinkedHashMap<EAName, EAValType> map = new LinkedHashMap<>();
		map.put(x, tf.val.unit());
		Gamma gamma = new Gamma(map, new LinkedHashMap<>());
		System.out.println("Typing hB: " + hsB.type(gamma));

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
