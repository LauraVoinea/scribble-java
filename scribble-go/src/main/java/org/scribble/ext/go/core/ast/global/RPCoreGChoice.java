package org.scribble.ext.go.core.ast.global;

import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.scribble.ast.global.GProtocolDecl;
import org.scribble.ext.go.core.ast.RPCoreAstFactory;
import org.scribble.ext.go.core.ast.RPCoreChoice;
import org.scribble.ext.go.core.ast.RPCoreSyntaxException;
import org.scribble.ext.go.core.ast.RPCoreType;
import org.scribble.ext.go.core.ast.local.RPCoreLActionKind;
import org.scribble.ext.go.core.ast.local.RPCoreLType;
import org.scribble.ext.go.core.type.RPAnnotatedInterval;
import org.scribble.ext.go.core.type.RPIndexedRole;
import org.scribble.ext.go.core.type.RPInterval;
import org.scribble.ext.go.core.type.RPRoleVariant;
import org.scribble.ext.go.core.type.name.RPCoreGDelegationType;
import org.scribble.ext.go.main.GoJob;
import org.scribble.ext.go.type.index.RPBinIndexExpr.Op;
import org.scribble.ext.go.type.index.RPIndexExpr;
import org.scribble.ext.go.type.index.RPIndexFactory;
import org.scribble.ext.go.type.index.RPIndexSelf;
import org.scribble.ext.go.type.index.RPIndexVar;
import org.scribble.ext.go.util.Z3Wrapper;
import org.scribble.type.Message;
import org.scribble.type.MessageSig;
import org.scribble.type.kind.Global;
import org.scribble.type.name.GDelegationType;
import org.scribble.type.name.PayloadElemType;
import org.scribble.type.name.Role;

public class RPCoreGChoice extends RPCoreChoice<RPCoreGType, Global> implements RPCoreGType
{
	public final RPIndexedRole src;  // "Singleton" -- checked by isWellFormed
	public final RPIndexedRole dest;  // this.dest == super.role -- arbitrary?

	public RPCoreGChoice(RPIndexedRole src, RPCoreGActionKind kind, RPIndexedRole dest, 
			//LinkedHashMap<RPCoreMessage, RPCoreGType> cases)
			LinkedHashMap<Message, RPCoreGType> cases)
	{
		super(dest, kind, cases);
		this.src = src;
		this.dest = dest;
	}
	
	@Override
	public RPCoreGType subs(RPCoreAstFactory af, RPCoreType<Global> old, RPCoreType<Global> neu)
	{
		if (this.equals(old))
		{
			return (RPCoreGType) neu;
		}
		else
		{
			LinkedHashMap<Message, RPCoreGType> tmp = new LinkedHashMap<>();
			this.cases.forEach((k, v) -> tmp.put(k, v.subs(af, old, neu)));  // Immutable, so neu can be shared by multiple cases
			return af.ParamCoreGChoice(this.src, RPCoreGActionKind.CROSS_TRANSFER, this.dest, tmp);
		}
	}
	
	@Override
	public boolean isWellFormed(GoJob job, GProtocolDecl gpd)
	{
		// src (i.e., choice subj) range size=1 for non-unary choices enforced by ParamScribble.g syntax
		// Directed choice check by ParamCoreGProtocolDeclTranslator ensures all dests (including ranges) are (syntactically) the same
		
		RPInterval srcRange = this.src.getParsedRange();
		RPInterval destRange = this.dest.getParsedRange();
		Set<RPIndexVar> vars = Stream.of(srcRange, destRange).flatMap(r -> r.getIndexVars().stream()).collect(Collectors.toSet());
		
		/*// CHECKME: is range size>0 already ensured by syntax?
		Function<ParamRange, String> foo1 = r -> 
				  "(assert (exists ((foobartmp Int)"
				+ vars.stream().map(v -> " (" + v.name + " Int)").collect(Collectors.joining(""))
				+ ") (and"
				+ " (>= foobartmp " + r.start.toSmt2Formula() + ") (<= foobartmp " + r.end.toSmt2Formula() + ")"  // FIXME: factor out with above
				+ ")))";
		Predicate<ParamRange> foo2 = r ->
		{
			String foo = foo1.apply(srcRange);

			job.debugPrintln("\n[param-core] [WF] Checking non-empty ranges:\n  " + foo);

			return Z3Wrapper.checkSat(job, gpd, foo);
		};*/
		Function<RPInterval, String> foo1 = r ->  // FIXME: factor out with above
				  "(assert "
				+ (vars.isEmpty() ? "" : "(exists (" + vars.stream().map(v -> "(" + v.name + " Int)").collect(Collectors.joining(" ")) + ") (and (and")
				+ vars.stream().map(v -> " (>= " + v + " 1)").collect(Collectors.joining(""))  // FIXME: lower bound constant -- replace by global invariant
				+ (vars.isEmpty() ? "" : ")")
				+ " (> " + r.start.toSmt2Formula() + " " + r.end.toSmt2Formula() + ")"
				+ (vars.isEmpty() ? "" : "))")
				+ ")";
		Predicate<RPInterval> foo2 = r ->
		{
			String foo = foo1.apply(r);

			job.debugPrintln("\n[param-core] [WF] Checking WF range interval for " + r + ":\n  " + foo); 

			return Z3Wrapper.checkSat(job, gpd, foo);
		};
		if (foo2.test(srcRange) || foo2.test(destRange))
		{
			return false;
		}


		if (this.kind == RPCoreGActionKind.CROSS_TRANSFER)
		{
			if (this.cases.size() > 1)
			{
				String bar = "(assert "
						+ (vars.isEmpty() ? "" : "(exists (" + vars.stream().map(v -> "(" + v.name + " Int)").collect(Collectors.joining(" ")) + ") (and ")
						+ vars.stream().map(v -> " (>= " + v + " 1)").collect(Collectors.joining(""))  // FIXME: lower bound constant -- replace by global invariant
						+ "(not (= (- " + srcRange.end.toSmt2Formula() + " " + srcRange.start.toSmt2Formula() + ") 0))"
						+ (vars.isEmpty() ? "" : "))")
						+ ")";

				job.debugPrintln("\n[param-core] [WF] Checking singleton choice-subject for " + this.src + ":\n  " + bar); 

				if (Z3Wrapper.checkSat(job, gpd, bar))
				{
					return false;
				}
			}
		}
		
		
		if (this.src.getName().equals(this.dest.getName()))
		{
			if (this.kind == RPCoreGActionKind.CROSS_TRANSFER)
			{
				String smt2 = "(assert (exists ((foobartmp Int)";  // FIXME: factor out
				smt2 += vars.stream().map(v -> " (" + v.name + " Int)").collect(Collectors.joining(""));
				smt2 += ") (and";
				smt2 += vars.isEmpty() ? "" : vars.stream().map(v -> " (>= " + v + " 1)").collect(Collectors.joining(""));  // FIXME: lower bound constant -- replace by global invariant
				smt2 += Stream.of(srcRange, destRange)
						.map(r -> " (>= foobartmp " + r.start.toSmt2Formula() + ") (<= foobartmp " + r.end.toSmt2Formula() + ")")
						.collect(Collectors.joining());
				smt2 += ")))";
				
				job.debugPrintln("\n[param-core] [WF] Checking non-overlapping ranges for " + this.src.getName() + ":\n  " + smt2);
				
				if (Z3Wrapper.checkSat(job, gpd, smt2))
				{
					return false;
				}
				// CHECKME: projection cases for rolename self-comm but non-overlapping intervals
			}
		}

		if (this.kind == RPCoreGActionKind.DOT_TRANSFER)
		{
			String smt2 = "(assert"
					+ (vars.isEmpty() ? "" : " (forall (" + vars.stream().map(v -> "(" + v + " Int)").collect(Collectors.joining(" "))) + ") "
					+ "(and (= (- " + srcRange.end.toSmt2Formula() + " " + srcRange.start.toSmt2Formula() + ") (- "
							+ destRange.end.toSmt2Formula() + " " + destRange.start.toSmt2Formula() + "))"
					+ (!this.src.getName().equals(this.dest.getName()) ? "" :
						" (not (= " + srcRange.start.toSmt2Formula() + " " + destRange.start.toSmt2Formula() + "))")
				  + ")"
					+ (vars.isEmpty() ? "" : ")")
					+ ")";
			
			job.debugPrintln("\n[param-core] [WF] Checking dot-range alignment between " + srcRange + " and " + destRange + ":\n  " + smt2);
			
			if (!Z3Wrapper.checkSat(job, gpd, smt2))
			{
				return false;
			}
		}

		return true;
	}

	@Override
	public RPCoreGActionKind getKind()
	{
		return (RPCoreGActionKind) this.kind;
	}
	
	@Override
	public Set<RPIndexedRole> getIndexedRoles()
	{
		Set<RPIndexedRole> res = Stream.of(this.src, this.dest).collect(Collectors.toSet());
		this.cases.values().forEach(c -> res.addAll(c.getIndexedRoles()));
		return res;
	}

	@Override
	public RPCoreLType project(RPCoreAstFactory af, //Role r, Set<ParamRange> ranges) throws ParamCoreSyntaxException
				RPRoleVariant subj) throws RPCoreSyntaxException
	{
		//LinkedHashMap<RPCoreMessage, RPCoreLType>
		LinkedHashMap<Message, RPCoreLType>
				projs = new LinkedHashMap<>();
		//for (Entry<RPCoreMessage, RPCoreGType>
		for (Entry<Message, RPCoreGType>
				e : this.cases.entrySet())
		{
			//RPCoreMessage a = e.getKey();
			Message a = e.getKey();
			//projs.put(a, e.getValue().project(af, r, ranges));
			projs.put(a, e.getValue().project(af, subj));
					// N.B. local actions directly preserved from globals -- so core-receive also has assertion (cf. ParamGActionTransfer.project, currently no ParamLReceive)
					// FIXME: receive assertion projection -- should not be the same as send?
			
			if (a instanceof MessageSig)
			{
				for (PayloadElemType<?> pet : //a.pay.elems)
						((MessageSig) a).payload.elems)
				{
					if (pet instanceof GDelegationType)
					{
						if (!(pet instanceof RPCoreGDelegationType))
						{
							throw new RuntimeException("[rp-core] TODO: " + pet);
						}
						RPCoreGDelegationType gdt = (RPCoreGDelegationType) pet;  // Payload types come from ParamCoreGProtocolDeclTranslator#parsePayload (toMessage)
						
						// cf. GDelegationElem#project
						
						//new LProtocolName();  // FIXME: need actual role (not just role name)
					}
				}
			}
			// MessageSigName is not delegation  // FIXME: cf. "Sync@A"
		}
		
		// "Simple" cases
		Role srcName = this.src.getName();
		Role destName = this.dest.getName();
		Role subjName = subj.getName();
		RPInterval srcRange = this.src.getParsedRange();
		RPInterval destRange = this.dest.getParsedRange();
		if (this.kind == RPCoreGActionKind.CROSS_TRANSFER)
		{	
			System.out.println("CCC1: " + this + ", " + subj);
			System.out.println("CCC2: " + srcName + ", " + destName + ", " + subjName);
			
			//if (this.src.getName().equals(r))
			if (srcName.equals(subjName) && subj.intervals.contains(srcRange))  // FIXME: factor out?
			{
				return af.ParamCoreLCrossChoice(this.dest, RPCoreLActionKind.CROSS_SEND, projs);
			}
			else if (destName.equals(subjName) && subj.intervals.contains(destRange))
			{
				return af.ParamCoreLCrossChoice(this.src, RPCoreLActionKind.CROSS_RECEIVE, projs);
			}
		}
		/*else if (this.kind == RPCoreGActionKind.DOT_TRANSFER)
		{
			if (srcName.equals(subjName) && subj.intervals.contains(srcRange))  // FIXME: factor out?
			{
				if (destName.equals(subjName) && subj.intervals.contains(destRange))  // Possible for dot-transfer (src.start != dest.start) -- cf. cross-transfer
				{
					RPIndexExpr offset = RPIndexFactory.ParamBinIndexExpr(RPBinIndexExpr.Op.Add,
								RPIndexFactory.ParamIntVar("_id"),
								RPIndexFactory.ParamBinIndexExpr(RPBinIndexExpr.Op.Subt, destRange.start, srcRange.start));
					/*Map<ParamCoreMessage, ParamCoreLType> tmp = projs.entrySet().stream().collect(Collectors.toMap(Entry::getKey,
							p -> new ParamCoreLDotChoice(this.dest, offset, ParamCoreLActionKind.DOT_SEND,
									Stream.of(p.getKey()).collect(Collectors.toMap(k -> k, k -> p.getValue())))
							));* /
					Function<Entry<RPCoreMessage, RPCoreLType>, LinkedHashMap<RPCoreMessage, RPCoreLType>> foo = e ->
					{
						LinkedHashMap<RPCoreMessage, RPCoreLType> res = new LinkedHashMap<>();
						res.put(e.getKey(), e.getValue());
						return res;
					};
					LinkedHashMap<RPCoreMessage, RPCoreLType> tmp = new LinkedHashMap<>();
					projs.entrySet().forEach(e -> tmp.put(e.getKey(), 
							new RPCoreLDotChoice(this.dest, offset, RPCoreLActionKind.DOT_SEND, foo.apply(e))));
					RPIndexExpr offset2 = RPIndexFactory.ParamBinIndexExpr(RPBinIndexExpr.Op.Add,
							RPIndexFactory.ParamIntVar("_id"),
						 RPIndexFactory.ParamBinIndexExpr(RPBinIndexExpr.Op.Subt, srcRange.start, destRange.start));
					return af.ParamCoreLDotChoice(this.src, offset2, RPCoreLActionKind.DOT_RECEIVE, tmp);
				}
				else
				{
					RPIndexExpr offset = RPIndexFactory.ParamBinIndexExpr(RPBinIndexExpr.Op.Add,
							RPIndexFactory.ParamIntVar("_id"),
							RPIndexFactory.ParamBinIndexExpr(RPBinIndexExpr.Op.Subt, destRange.start, srcRange.start));
					return af.ParamCoreLDotChoice(this.dest, offset, RPCoreLActionKind.DOT_SEND, projs);
				}
			}
			else if (destName.equals(subjName) && subj.intervals.contains(destRange))
			{
				RPIndexExpr offset = RPIndexFactory.ParamBinIndexExpr(RPBinIndexExpr.Op.Add,
						RPIndexFactory.ParamIntVar("_id"),
						RPIndexFactory.ParamBinIndexExpr(RPBinIndexExpr.Op.Subt, srcRange.start, destRange.start));
				return af.ParamCoreLDotChoice(this.src, offset, RPCoreLActionKind.DOT_RECEIVE, projs);
			}
		}*/
		else
		{
			throw new RuntimeException("[param-core] TODO: " + this);
		}
		
		// src name != dest name
		//return merge(af, r, ranges, projs);
		return merge(af, subj, projs);
	}

	// Duplicated from project above
	@Override
	public RPCoreLType project3(RPCoreAstFactory af, Set<Role> roles, Set<RPAnnotatedInterval> ivals, RPIndexedRole subj) throws RPCoreSyntaxException
	{
		LinkedHashMap<Message, RPCoreLType> projs = new LinkedHashMap<>();
		for (Entry<Message, RPCoreGType> e : this.cases.entrySet())
		{
			Message a = e.getKey();
			projs.put(a, e.getValue().project3(af, roles, ivals, subj));
		}
		
		Role srcName = this.src.getName();
		Role destName = this.dest.getName();
		Role subjName = subj.getName();
		RPInterval srcRange = this.src.getParsedRange();
		RPInterval destRange = this.dest.getParsedRange();
		Set<String> fvars = ivals.stream().map(x -> x.var.toString()).collect(Collectors.toSet());
		if (this.kind == RPCoreGActionKind.CROSS_TRANSFER)
		{	
			
			System.out.println("DDD4: " + this.src + " ,, " + this.dest + " ,, " + subj);
			System.out.println("DDD5: " + this.dest.equals(subj));
			
			if (this.src.equals(subj))  // N.B. subj uses RPIndexVar (not RPForeachVar) -- cf. the invocation of project3 in RPCoreGForeach#project2
			{
				//return af.ParamCoreLCrossChoice(this.dest, RPCoreLActionKind.CROSS_SEND, projs);
				if (srcRange.start.equals(srcRange.end) //&& srcRange.start instanceof RPIndexVar
						&& destRange.start.equals(destRange.end) //&& destRange.start instanceof RPIndexVar
						)
				{
					RPIndexExpr srcStart = srcRange.start;
					RPIndexExpr destStart = destRange.start;
					Set<RPIndexExpr> tmp = Stream.of(srcStart, destStart).filter(x -> x instanceof RPIndexVar).map(x -> (RPIndexVar) x).collect(Collectors.toSet());
					if (roles.contains(destName) && //fvars.contains(srcVar.toString()) && fvars.contains(destVar.toString()))
							fvars.containsAll(tmp))
					{
						RPIndexExpr destExpr = RPIndexFactory.ParamBinIndexExpr(Op.Add, RPIndexSelf.SELF, RPIndexFactory.ParamBinIndexExpr(Op.Subt, destStart, srcStart));
						RPIndexedRole dest = new RPIndexedRole(destName.toString(), Stream.of(new RPInterval(destExpr, destExpr)).collect(Collectors.toSet()));
						return af.ParamCoreLCrossChoice(dest, RPCoreLActionKind.CROSS_SEND, projs);
					}
					else if (!roles.contains(destName) || !(fvars.contains(srcStart.toString()) && fvars.contains(destStart.toString())))
					{
						return af.ParamCoreLCrossChoice(this.dest, RPCoreLActionKind.CROSS_SEND, projs);
					}
				}
				// Otherwise try merge (if neither src/dest are subj)
			}
			else if (this.dest.equals(subj))
			{
				
				System.out.println("DDD6: " + " " + srcRange.start + " ,, " + srcRange.end + " ,, " + srcRange.start.equals(srcRange.end) + " ,, " + srcRange.start.getClass());
				
				if (srcRange.start.equals(srcRange.end) //&& srcRange.start instanceof RPIndexVar
						&& destRange.start.equals(destRange.end) //&& destRange.start instanceof RPIndexVar)
						)
				{
					RPIndexExpr srcStart = srcRange.start;
					RPIndexExpr destStart = destRange.start;
					Set<RPIndexVar> tmp = Stream.of(srcStart, destStart).filter(x -> x instanceof RPIndexVar).map(x -> (RPIndexVar) x).collect(Collectors.toSet());
					
					System.out.println("DDD7: " + " ,, " + fvars + " ,, " + srcStart + " ,, " + destStart);
					
					if (roles.contains(destName) && //fvars.contains(srcVar.toString()) && fvars.contains(destVar.toString()))
							fvars.containsAll(tmp))
					{
						RPIndexExpr srcExpr = RPIndexFactory.ParamBinIndexExpr(Op.Add, RPIndexSelf.SELF, RPIndexFactory.ParamBinIndexExpr(Op.Subt, srcStart, destStart));
						RPIndexedRole src = new RPIndexedRole(destName.toString(), Stream.of(new RPInterval(srcExpr, srcExpr)).collect(Collectors.toSet()));
						return af.ParamCoreLCrossChoice(src, RPCoreLActionKind.CROSS_SEND, projs);
					}
					else
					{
						return af.ParamCoreLCrossChoice(this.src, RPCoreLActionKind.CROSS_RECEIVE, projs);
					}
				}
				// Otherwise merge (if neither src/dest are subj)
			}
		}
		else
		{
			throw new RuntimeException("[param-core] TODO: " + this);
		}
		
		// Not CROSS_TRANSFER, or subj is not sender nor receiver
		if (!this.src.equals(subj) && !this.src.equals(subj))
		{
			return merge3(af, subj, projs);
		}
		else
		{
			throw new RuntimeException("[rp-core] Projection not defined: " + this + ", " + subj);
		}
	}
		
	//private ParamCoreLType merge(ParamCoreAstFactory af, Role r, Set<ParamRange> ranges, Map<ParamCoreMessage, ParamCoreLType> projs) throws ParamCoreSyntaxException
	private RPCoreLType merge(RPCoreAstFactory af, RPRoleVariant r, 
			//Map<RPCoreMessage, RPCoreLType> projs) throws RPCoreSyntaxException
			Map<Message, RPCoreLType> projs) throws RPCoreSyntaxException
	{
		// "Merge"
		Set<RPCoreLType> values = new HashSet<>(projs.values());
		if (values.size() > 1)
		{
			throw new RPCoreSyntaxException("[param-core] Cannot project \n" + this + "\n onto " + r 
					//+ " for " + ranges
					+ ": cannot merge for: " + projs.keySet());
		}
		
		return values.iterator().next();
	}

	// Cf. merge above
	private RPCoreLType merge3(RPCoreAstFactory af, RPIndexedRole r, Map<Message, RPCoreLType> projs) throws RPCoreSyntaxException
	{
		Set<RPCoreLType> values = new HashSet<>(projs.values());
		if (values.size() > 1)
		{
			throw new RPCoreSyntaxException("[param-core] Cannot project \n" + this + "\n onto " + r + ": cannot merge for: " + projs.keySet());
		}
		return values.iterator().next();
	}
	
	@Override
	public int hashCode()
	{
		int hash = 2339;
		hash = 31 * hash + super.hashCode();
		hash = 31 * hash + this.src.hashCode();
		return hash;
	}

	@Override
	public boolean equals(Object obj)
	{
		if (this == obj)
		{
			return true;
		}
		if (!(obj instanceof RPCoreGChoice))
		{
			return false;
		}
		return super.equals(obj) && this.src.equals(((RPCoreGChoice) obj).src);  // Does canEquals
	}
	
	@Override
	public boolean canEquals(Object o)
	{
		return o instanceof RPCoreGChoice;
	}

	@Override
	public String toString()
	{
		return this.src.toString() + this.kind + this.dest + casesToString();  // toString needed?
	}
}
