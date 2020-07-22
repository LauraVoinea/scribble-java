package org.scribble.ext.assrt.ast.global;

import java.util.Collections;
import java.util.List;

import org.antlr.runtime.Token;
import org.scribble.ast.NonRoleArgList;
import org.scribble.ast.RoleArgList;
import org.scribble.ast.global.GDo;
import org.scribble.ast.name.qualified.GProtoNameNode;
import org.scribble.ast.name.qualified.ProtoNameNode;
import org.scribble.core.type.kind.Global;
import org.scribble.del.DelFactory;
import org.scribble.ext.assrt.ast.AssrtAExprNode;
import org.scribble.ext.assrt.ast.AssrtStateVarArgList;
import org.scribble.ext.assrt.ast.AssrtStateVarArgNode;
import org.scribble.ext.assrt.del.AssrtDelFactory;
import org.scribble.util.ScribException;
import org.scribble.visit.AstVisitor;

// Cf. AssrtGContinue
public class AssrtGDo extends GDo implements AssrtStateVarArgNode
{
	public static final int STATEVAR_ARG_LIST_CHILD_INDEX = 3;  // null if none

	// ScribTreeAdaptor#create constructor
	public AssrtGDo(Token t)
	{
		super(t);
	}

	// Tree#dupNode constructor
	public AssrtGDo(AssrtGDo node)
	{
		super(node);
	}

	public AssrtStateVarArgList getStateVarArgListChild()
	{
		return (AssrtStateVarArgList) getChild(STATEVAR_ARG_LIST_CHILD_INDEX);
	}

	@Override
	public List<AssrtAExprNode> getAnnotExprChildren()
	{
		/*List<? extends ScribNode> cs = getChild(STATEVAREXPR_CHILDREN_START_INDEX)
				.getChildren();
		return cs.stream()
				.map(x -> (AssrtAExprNode) x).collect(Collectors.toList());*/
		AssrtStateVarArgList sexprs = getStateVarArgListChild();
		return sexprs == null ? Collections.emptyList()
				: sexprs.getAnnotExprChildren();
	}

	// "add", not "set"
	public void addScribChildren(ProtoNameNode<Global> proto, NonRoleArgList as,
			RoleArgList rs, //List<AssrtAExprNode> sexprs)
			AssrtStateVarArgList sexprs)  // May be null
	{
		// Cf. above getters and Scribble.g children order
		super.addScribChildren(proto, as, rs);
		//addChildren(sexprs);
		addChild(sexprs);
	}
	
	@Override
	public AssrtGDo dupNode()
	{
		return new AssrtGDo(this);
	}
	
	@Override
	public void decorateDel(DelFactory df)
	{
		((AssrtDelFactory) df).AssrtGDo(this);
	}

	public AssrtGDo reconstruct(ProtoNameNode<Global> proto, RoleArgList rs,
			NonRoleArgList as, //List<AssrtAExprNode> sexprs)
			AssrtStateVarArgList sexprs)  // May be null
	{
		AssrtGDo dup = dupNode();
		dup.addScribChildren(proto, as, rs, sexprs);
		dup.setDel(del());  // No copy
		return dup;
	}

	@Override
	public AssrtGDo visitChildren(AstVisitor v) throws ScribException
	{
		GProtoNameNode proto = visitChildWithClassEqualityCheck(this,
				getProtoNameChild(), v);
		RoleArgList rs = (RoleArgList) visitChild(getRoleListChild(), v);
		NonRoleArgList as = (NonRoleArgList) visitChild(getNonRoleListChild(), v);
		//List<AssrtAExprNode> sexprs = visitChildListWithClassEqualityCheck(this, getAnnotExprChildren(), v);  // Supports empty list
		AssrtStateVarArgList sexprs = getStateVarArgListChild();
		if (sexprs != null)
		{
			sexprs = (AssrtStateVarArgList) visitChild(sexprs, v);
		}
		return reconstruct(proto, rs, as, sexprs);
	}

	@Override
	public String toString()
	{
		AssrtStateVarArgList sexprs = getStateVarArgListChild();
		return super.toString() //+ annotToString();
				+ " " + (sexprs != null ? sexprs : "");
	}
}














/*
	public final List<AssrtArithExpr> exprs;
	
	public AssrtGDo(CommonTree source, RoleArgList roles, NonRoleArgList args,
			GProtoNameNode proto)
	{
		this(source, roles, args, proto, //null);
				Collections.emptyList());
	}

	public AssrtGDo(CommonTree source, RoleArgList roles, NonRoleArgList args,
			GProtoNameNode proto, List<AssrtArithExpr> exprs)
	{
		super(source, roles, args, proto);
		//this.annot = annot;
		this.exprs = Collections.unmodifiableList(exprs);
	}

	public LDo project(AstFactory af, Role self, LProtocolNameNode fullname)
	{
		throw new RuntimeException("[assrt] Shouldn't get in here: " + this);
	}

	public LDo project(AstFactory af, Role self, LProtocolNameNode fullname, //AssrtArithExpr annot)
			List<AssrtArithExpr> annotexprs)
	{
		RoleArgList roleinstans = this.roles.project(af, self);
		NonRoleArgList arginstans = this.args.project(af, self);
		AssrtLDo ld = ((AssrtAstFactory) af).AssrtLDo(this.source, roleinstans, arginstans, fullname, //annot);
				annotexprs);
		return ld;
	}
*/