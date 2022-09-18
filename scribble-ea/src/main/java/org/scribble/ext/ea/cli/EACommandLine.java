package org.scribble.ext.ea.cli;

import org.scribble.cli.CommandLine;
import org.scribble.cli.CommandLineException;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.config.*;
import org.scribble.ext.ea.core.process.*;
import org.scribble.ext.ea.core.type.EAType;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.Delta;
import org.scribble.ext.ea.core.type.session.local.EALInType;
import org.scribble.ext.ea.core.type.session.local.EALOutType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.util.EAPPair;
import org.scribble.ext.ea.util.EATriple;
import org.scribble.util.AntlrSourceException;
import org.scribble.util.Pair;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;



// HERE
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

		EAPRuntimeFactory rf = EAPRuntimeFactory.factory;
		EAPFactory f = EAPFactory.factory;
		EATypeFactory tf = EATypeFactory.factory;

		//ex1(rf, f);
		ex2(rf, f, tf);

		//new EACommandLine(args).run();
	}

	private static void ex2(EAPRuntimeFactory rf, EAPFactory f, EATypeFactory tf) {
		Role A = new Role("A");
		Role B = new Role("B");
		Op l1 = new Op("l1");
		Op l2 = new Op("l2");
		EAPUnit unit = f.unit();
		EAPSid s = rf.sid("s");
		EAPPid p1 = rf.pid("p1");
		EAPPid p2 = rf.pid("p2");
		EAPVar x = f.var("x");

		// let x = B!l1() in B!l2()
		EAPSend sendAB1 = f.send(B, l1, unit);
		EAPSend sendAB2 = f.send(B, l2, unit);

		EAPLet let = f.let(x, tf.val.unit(), sendAB1, sendAB2);

		LinkedHashMap<Op, Pair<EAValType, EALType>> cases = new LinkedHashMap<>();
		cases.put(l2, new Pair<>(tf.val.unit(), tf.local.end()));
		EALOutType out2 = tf.local.out(B, cases);
		cases = new LinkedHashMap<>();
		cases.put(l1, new Pair<>(tf.val.unit(), out2));
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
		EAPReturn ret = f.returnn(f.unit());
		Hs.put(l2, new EATriple<>(x, tf.val.unit(), ret));
		EAPHandlers hB2 = f.handlers(B, Hs);

		Hs = new LinkedHashMap<>();
		EAPSuspend sus = f.suspend(hB2);  // XXX suspend
		// l1 -> (x, continuation)
		Hs.put(l1, new EATriple<>(x, tf.val.unit(), sus));
		EAPHandlers hB1 = f.handlers(B, Hs);

		LinkedHashMap<EAName, EAValType> map = new LinkedHashMap<>();
		map.put(x, tf.val.unit());
		Gamma gamma = new Gamma(map);
		System.out.println("Typing hB: " + hB1.type(gamma));

		LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaB = new LinkedHashMap<>();
		sigmaB.put(new EAPPair<>(s, B), hB1);  // !!! TODO make sigma concrete, e.g., for typing
		EAPConfig cB = rf.config(p2, idle, sigmaB);

		cases = new LinkedHashMap<>();
		cases.put(l2, new Pair<>(tf.val.unit(), tf.local.end()));
		EALInType in2 = tf.local.in(B, cases);
		cases = new LinkedHashMap<>();
		cases.put(l1, new Pair<>(tf.val.unit(), in2));
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

		EAPSystem sys = rf.system(cs);
		System.out.println(sys);
		/*Map<EAPPid, EAPConfig> cfgs = sys.getConfigs();
		//System.out.println("Typing p1/A: " + cfgs.get(p1));
		//cfgs.get(p1).type(new Gamma(), new Delta(env));  // TODO env for p1/A
		System.out.println("Typing p2/B: " + cfgs.get(p2));
		cfgs.get(p2).type(new Gamma(), new Delta(env));*/
		env.put(new EAPPair<>(s, A), out1);
		System.out.println(env);
		sys.type(new Gamma(), new Delta(), new Delta(env));

		System.out.println();
		sys = sys.reduce(p1);
		System.out.println(sys);
		env.put(new EAPPair<>(s, A), out2);
		env.put(new EAPPair<>(s, B), in2);
		System.out.println(env);
		sys.type(new Gamma(), new Delta(), new Delta(env));

		sys = sys.reduce(p1);
		System.out.println();
		System.out.println(sys);

		sys = sys.reduce(p2);
		System.out.println();
		System.out.println(sys);

		sys = sys.reduce(p1);
		System.out.println();
		System.out.println(sys);

		sys = sys.reduce(p1);
		System.out.println();
		System.out.println(sys);

		sys = sys.reduce(p2);
		System.out.println();
		System.out.println(sys);
	}

	/*
	private static void ex1(EAPRuntimeFactory rf, EAPFactory f) {
		Role A = new Role("A");
		Role B = new Role("B");
		Op l1 = new Op("l1");
		EAPUnit unit = f.unit();
		EAPSid s = rf.sid("s");
		EAPPid p1 = rf.pid("p1");
		EAPPid p2 = rf.pid("p2");

		// A: B!l1(unit)
		EAPSend sendAB = f.send(B, l1, unit);
		EAPActiveThread tA = rf.activeThread(sendAB, s, A);
		LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaA = new LinkedHashMap<>();
		EAPConfig cA = rf.config(p1, tA, sigmaA);

		// idle, s[B] |-> handler B { l1(x) |-> return(unit) }  -- l1 |-> (x, return(unit)
		LinkedHashMap<Op, Pair<EAPVar, EAPExpr>> Hs = new LinkedHashMap<>();
		EAPVar x = f.var("x");
		EAPReturn ret = f.returnn(unit);
		Hs.put(l1, new EAPPair<EAPVar, EAPExpr>(x, ret));
		EAPHandlers hB = f.handlers(B, Hs);
		EAPIdle idle = rf.idle();
		LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaB = new LinkedHashMap<>();
		sigmaB.put(new EAPPair<>(s, B), hB);
		EAPConfig cB = rf.config(p2, idle, sigmaB);

		System.out.println(cA);
		System.out.println(cB);

		LinkedHashMap<EAPPid, EAPConfig> cs = new LinkedHashMap<>();
		cs.put(p1, cA);
		cs.put(p2, cB);
		EAPSystem sys = rf.system(cs);

		sys = sys.reduce(p1);
		System.out.println();
		System.out.println(sys);

		sys = sys.reduce(p1);
		System.out.println();
		System.out.println(sys);

		sys = sys.reduce(p2);
		System.out.println();
		System.out.println(sys);

		/*sys = sys.reduce(p2);
		System.out.println();
		System.out.println(sys);* /
	}
	//*/




}
