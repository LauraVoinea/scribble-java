package org.scribble.ext.assrt.core.type.session.global;

import java.util.LinkedHashMap;
import java.util.List;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.core.type.kind.Global;
import org.scribble.core.type.kind.NonRoleParamKind;
import org.scribble.core.type.name.ProtoName;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Arg;
import org.scribble.core.type.session.Msg;
import org.scribble.core.type.session.global.GContinue;
import org.scribble.core.type.session.global.GDisconnect;
import org.scribble.core.type.session.global.GDo;
import org.scribble.core.type.session.global.GMessageTransfer;
import org.scribble.core.type.session.global.GRecursion;
import org.scribble.core.type.session.global.GSeq;
import org.scribble.core.type.session.global.GType;
import org.scribble.core.type.session.global.GTypeFactoryImpl;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;


// CHECKME: override pattern not ideal, but maybe OK due to the "language shift" -- also no Impl suffix
public class AssrtGTypeFactory extends GTypeFactoryImpl
{

	// FIXME drop Core from name
	public AssrtGChoice AssrtCoreGChoice(CommonTree source, Role src,
			AssrtGActionKind kind, Role dst,
			LinkedHashMap<AssrtMsg, AssrtGType> cases)
	{
		return new AssrtGChoice(source, src, kind, dst, cases);
	}

	public AssrtGDo AssrtCoreGDo(CommonTree source, ProtoName<Global> proto,
			List<Role> roles, List<Arg<? extends NonRoleParamKind>> args,
			List<AssrtAFormula> sexprs)
	{
		return new AssrtGDo(source, proto, roles, args, sexprs);
	}
	
	public AssrtGRec AssrtCoreGRec(CommonTree source, RecVar rv,
			AssrtGType body, LinkedHashMap<AssrtVar, AssrtAFormula> svars,
			AssrtBFormula ass, LinkedHashMap<AssrtVar, Role> located,
			LinkedHashMap<AssrtVar, AssrtAFormula> phantom)
	{
		return new AssrtGRec(source, rv, body, svars, ass, located, phantom);
	}
	
	public AssrtGRecVar AssrtCoreGRecVar(CommonTree source, RecVar rv,
			List<AssrtAFormula> sexprs)
	{
		return new AssrtGRecVar(source, rv, sexprs);
	}

	public AssrtGEnd AssrtCoreGEnd()
	{
		return AssrtGEnd.END;
	}

	@Override
	public org.scribble.core.type.session.global.GChoice GChoice(
			CommonTree source, Role subj,
			List<org.scribble.core.type.session.global.GSeq> blocks)
	{
		throw new RuntimeException(
				"Deprecated for " + getClass() + ":\n\t" + source);
	}

	@Override
	public org.scribble.core.type.session.global.GConnect GConnect(
			CommonTree source, Role src, Msg msg, Role dst)
	{
		throw new RuntimeException(
				"Deprecated for " + getClass() + ":\n\t" + source);
	}

	@Override
	public GContinue GContinue(
			CommonTree source, RecVar rv)
	{
		throw new RuntimeException(
				"Deprecated for " + getClass() + ":\n\t" + source);
	}

	@Override
	public GDisconnect GDisconnect(
			CommonTree source, Role left, Role right)
	{
		throw new RuntimeException(
				"Deprecated for " + getClass() + ":\n\t" + source);
	}

	@Override
	public GDo GDo(CommonTree source,
			ProtoName<Global> proto, List<Role> roles,
			List<Arg<? extends NonRoleParamKind>> args)
	{
		throw new RuntimeException(
				"Deprecated for " + getClass() + ":\n\t" + source);
	}

	@Override
	public GMessageTransfer GMessageTransfer(
			CommonTree source, Role src, Msg msg, Role dst)
	{
		throw new RuntimeException(
				"Deprecated for " + getClass() + ":\n\t" + source);
	}

	@Override
	public GRecursion GRecursion(CommonTree source, RecVar rv, GSeq body)
	{
		throw new RuntimeException(
				"Deprecated for " + getClass() + ":\n\t" + source);
	}

	@Override
	public GSeq GSeq(CommonTree source,
			List<GType> elems)
	{
		throw new RuntimeException(
				"Deprecated for " + getClass() + ":\n\t" + source);
	}
}
