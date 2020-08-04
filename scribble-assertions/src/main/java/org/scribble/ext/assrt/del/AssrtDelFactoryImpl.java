/**
 * Copyright 2008 The Scribble Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package org.scribble.ext.assrt.del;

import org.scribble.ast.global.GConnect;
import org.scribble.ast.global.GDo;
import org.scribble.ast.global.GMsgTransfer;
import org.scribble.ast.local.LRecv;
import org.scribble.ast.name.simple.AmbigNameNode;
import org.scribble.del.DelFactoryImpl;
import org.scribble.ext.assrt.ast.AssrtAExprNode;
import org.scribble.ext.assrt.ast.AssrtAnnotDataElem;
import org.scribble.ext.assrt.ast.AssrtBExprNode;
import org.scribble.ext.assrt.ast.AssrtModule;
import org.scribble.ext.assrt.ast.AssrtStateVarArgList;
import org.scribble.ext.assrt.ast.AssrtStateVarDecl;
import org.scribble.ext.assrt.ast.AssrtStateVarDeclList;
import org.scribble.ext.assrt.ast.AssrtStateVarHeaderAnnot;
import org.scribble.ext.assrt.ast.global.AssrtGConnect;
import org.scribble.ext.assrt.ast.global.AssrtGDo;
import org.scribble.ext.assrt.ast.global.AssrtGMsgTransfer;
import org.scribble.ext.assrt.ast.global.AssrtGProtoHeader;
import org.scribble.ext.assrt.ast.local.AssrtLDo;
import org.scribble.ext.assrt.ast.local.AssrtLProtoHeader;
import org.scribble.ext.assrt.ast.local.AssrtLReq;
import org.scribble.ext.assrt.ast.local.AssrtLSend;
import org.scribble.ext.assrt.ast.name.simple.AssrtVarNameNode;
import org.scribble.ext.assrt.del.global.AssrtGConnectDel;
import org.scribble.ext.assrt.del.global.AssrtGDoDel;
import org.scribble.ext.assrt.del.global.AssrtGMsgTransferDel;
import org.scribble.ext.assrt.del.local.AssrtLDoDel;
import org.scribble.ext.assrt.del.local.AssrtLRecvDel;
import org.scribble.ext.assrt.del.local.AssrtLReqDel;
import org.scribble.ext.assrt.del.local.AssrtLSendDel;
import org.scribble.ext.assrt.del.name.AssrtAmbigNameNodeDel;


public class AssrtDelFactoryImpl extends DelFactoryImpl implements AssrtDelFactory
{
	/**
	 * Existing (base) node classes with new dels
   * (Instantiating existing node classes with new dels)
	 */

	/* Names */

	@Override
	public void AmbigNameNode(AmbigNameNode n)
	{
		setDel(n, new AssrtAmbigNameNodeDel());
	}
	
	
	/* General and globals */

	// Cf/ AssrtScribble.g and AssrtScribTreeAdaptor
	@Override
	public void GMsgTransfer(GMsgTransfer n)
	{
		//setDel(n, new AssrtGMsgTransferDel());
		throw new RuntimeException("Deprecated");
	}
	
	@Override
	public void GConnect(GConnect n)
	{
		//setDel(n, new AssrtGConnectDel());
		throw new RuntimeException("Deprecated");
	}
	
	@Override
	public void GDo(GDo n)
	{
		//setDel(n, new AssrtGDoDel());
		throw new RuntimeException("Deprecated");
	}


	/* Locals */

	@Override
	public void LRecv(LRecv n)
	{
		setDel(n, new AssrtLRecvDel());  // CHECKME: AssrtLReceive with assertion?
	}
	

	/**
	 * New (Assrt) node types  
	 */
	
	/* Names */

	@Override
	public void AssrtIntVarNameNode(AssrtVarNameNode n)
	{
		setDel(n, createDefaultDel());
	}


	/* General and globals */

	@Override
	public void AssrtModule(AssrtModule n)
	{
		setDel(n, new AssrtModuleDel());
	}
	

	/**
	 * New (Assrt) node types
	 * (Explicitly creating new Assrt nodes)
	 */

	/* General and globals */

	@Override
	public void AssrtAssertion(AssrtBExprNode n)
	{
		setDel(n, createDefaultDel());
	}

	@Override
	public void AssrtArithExpr(AssrtAExprNode n)
	{
		setDel(n, createDefaultDel());
	}

	@Override
	public void AssrtGProtoHeader(AssrtGProtoHeader n)
	{
		setDel(n, createDefaultDel());  // Annots handled directly by AssrtAnnotationChecker Def enter/exit
	}
	
	@Override
	public void AssrtStateVarAnnotNode(AssrtStateVarHeaderAnnot n)
	{
		setDel(n, createDefaultDel());
	}

	@Override
	public void AssrtStateVarDeclList(AssrtStateVarDeclList n)
	{
		setDel(n, createDefaultDel());
	}

	@Override
	public void AssrtStateVarDecl(AssrtStateVarDecl n)
	{
		setDel(n, createDefaultDel());
	}

	@Override
	public void AssrtStateVarArgList(AssrtStateVarArgList n)
	{
		setDel(n, createDefaultDel());
	}

	@Override
	public void AssrtAnnotDataElem(AssrtAnnotDataElem n)
	{
		setDel(n, new AssrtAnnotDataTypeElemDel());
	}

	@Override
	public void AssrtGMsgTransfer(AssrtGMsgTransfer n)
	{
		setDel(n, new AssrtGMsgTransferDel());
	}

	@Override
	public void AssrtGDo(AssrtGDo n)
	{
		setDel(n, new AssrtGDoDel());
	}


	/* Locals */

	@Override
	public void AssrtLProtoHeader(AssrtLProtoHeader n)
	{
		setDel(n, createDefaultDel());  // Annots handled directly by AssrtAnnotationChecker Def enter/exit
	}

	@Override
	public void AssrtLSend(AssrtLSend n)
	{
		setDel(n, new AssrtLSendDel());
	}

	@Override
	public void AssrtLDo(AssrtLDo n)
	{
		setDel(n, new AssrtLDoDel());
	}

	/*
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	 */

	@Override
	public void AssrtGConnect(AssrtGConnect n)
	{
		setDel(n, new AssrtGConnectDel());
	}

	@Override
	public void AssrtLReq(AssrtLReq n)
	{
		setDel(n, new AssrtLReqDel());
	}
}

/*
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
  // TODO 

	@Override
	public void AssrtGContinue(AssrtGContinue n)
	{
		setDel(n, new AssrtGContinueDel());
	}

	@Override
	public void AssrtGRecursion(AssrtGRecursion n)
	{
		setDel(n, new AssrtGRecursionDel());
	}

	@Override
	public void AssrtLContinue(AssrtLContinue n)
	{
		setDel(n, new AssrtLContinueDel());
	}

	@Override
	public void AssrtLRecursion(AssrtLRecursion n)
	{
		setDel(n, new AssrtLRecursionDel());
	}
*/
