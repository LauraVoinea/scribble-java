package org.scribble.ext.assrt.ast;

import java.util.List;

import org.antlr.runtime.Token;
import org.scribble.ast.AstFactory;
import org.scribble.ast.ImportDecl;
import org.scribble.ast.ModuleDecl;
import org.scribble.ast.MsgNode;
import org.scribble.ast.NonProtoDecl;
import org.scribble.ast.NonRoleArgList;
import org.scribble.ast.NonRoleParamDeclList;
import org.scribble.ast.ProtoDecl;
import org.scribble.ast.RoleArgList;
import org.scribble.ast.RoleDeclList;
import org.scribble.ast.name.PayElemNameNode;
import org.scribble.ast.name.qualified.DataNameNode;
import org.scribble.ast.name.qualified.GProtoNameNode;
import org.scribble.ast.name.qualified.LProtoNameNode;
import org.scribble.ast.name.simple.RoleNode;
import org.scribble.core.type.kind.DataKind;
import org.scribble.ext.assrt.ast.global.AssrtGConnect;
import org.scribble.ext.assrt.ast.global.AssrtGDo;
import org.scribble.ext.assrt.ast.global.AssrtGMsgTransfer;
import org.scribble.ext.assrt.ast.global.AssrtGProtoHeader;
import org.scribble.ext.assrt.ast.local.AssrtLDo;
import org.scribble.ext.assrt.ast.local.AssrtLProtoHeader;
import org.scribble.ext.assrt.ast.local.AssrtLReq;
import org.scribble.ext.assrt.ast.local.AssrtLSend;
import org.scribble.ext.assrt.ast.name.simple.AssrtIntVarNameNode;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;


public interface AssrtAstFactory extends AstFactory
{
	/* Names */

	AssrtIntVarNameNode AssrtIntVarNameNode(Token t, String text);


	/* General and globals */

	AssrtAExprNode AssrtAExprNode(Token t, AssrtAFormula aform);  // Int expr
	AssrtBExprNode AssrtBExprNode(Token t, AssrtBFormula bform);  // Bool expr

	AssrtModule AssrtModule(Token t, ModuleDecl modd,
			List<? extends ImportDecl<?>> impds,
			List<? extends NonProtoDecl<?>> nprods,
			List<? extends ProtoDecl<?>> prods);

	AssrtGProtoHeader AssrtGProtoHeader(Token t, GProtoNameNode name,
			RoleDeclList rs, NonRoleParamDeclList ps, 
			AssrtStateVarDeclList svars, AssrtBExprNode ass);//, List<AssrtAExprNode> sexprs);  // FIXME: not actually how parsed
	AssrtStateVarDeclList AssrtStateVarDeclList(Token t,
			List<AssrtStateVarDecl> svars);

	AssrtStateVarDecl AssrtStateVarDecl(Token t, AssrtIntVarNameNode svar,
			PayElemNameNode<DataKind> data, AssrtAExprNode sexpr, RoleNode role);

	AssrtAnnotDataElem AssrtAnnotDataTypeElem(Token t,
			AssrtIntVarNameNode var, DataNameNode data);

	AssrtGMsgTransfer AssrtGMsgTransfer(Token t, MsgNode msg, RoleNode src,
			List<RoleNode> dsts, AssrtBExprNode ass);

	AssrtGDo AssrtGDo(Token t, GProtoNameNode proto, NonRoleArgList as,
			RoleArgList rs, //List<AssrtAExprNode> aexprs);
			AssrtStateVarArgList sexprs);

	/* Locals */

	AssrtLProtoHeader AssrtLProtoHeader(Token t, LProtoNameNode name,
			RoleDeclList rs, NonRoleParamDeclList ps, AssrtStateVarDeclList svars,
			AssrtBExprNode ass);

	AssrtLSend AssrtLSend(Token t, MsgNode msg, RoleNode self,
			List<RoleNode> dsts, AssrtBExprNode ass);

	AssrtLDo AssrtLDo(Token t, RoleArgList rs, NonRoleArgList as,
			LProtoNameNode proto, List<AssrtAExprNode> aexprs);


	/*
	 
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	  TODO */

	AssrtGConnect AssrtGConnect(Token t, MsgNode msg, RoleNode src, RoleNode dst,
			AssrtBExprNode ass);

	AssrtLReq AssrtLReq(Token t, MsgNode msg, RoleNode self, RoleNode dst,
			AssrtBExprNode ass);

}




/*
	@Deprecated  // Not currently parsed (or used)
	AssrtGContinue AssrtGContinue(Token t, RecVarNode rv,
			List<AssrtAExprNode> aexprs);

	@Deprecated  // Not currently parsed (or used)
	AssrtGRecursion AssrtGRecursion(Token t, RecVarNode rv, GProtoBlock block,
			AssrtBExprNode ass, List<AssrtIntVarNameNode> avars,
			List<AssrtAExprNode> aexprs);  // FIXME: not actually how parsed

	@Deprecated
	AssrtLContinue AssrtLContinue(Token t, RecVarNode rv, List<AssrtAExprNode> aexprs);

	@Deprecated
	AssrtLRecursion AssrtLRecursion(Token t, RecVarNode rv, LProtoBlock block,
			List<AssrtIntVarNameNode> avars, List<AssrtAExprNode> aexprs,
			AssrtBExprNode ass);  // FIXME: not actually how parsed
*/