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
package org.scribble.ext.gt.parser;

import org.antlr.runtime.Token;
import org.scribble.ast.ScribNode;
import org.scribble.ast.ScribNodeBase;
import org.scribble.del.DelFactory;
import org.scribble.ext.gt.ast.global.GTGMixed;
import org.scribble.parser.ScribAntlrTokens;
import org.scribble.parser.ScribTreeAdaptor;

public class GTScribTreeAdaptor extends ScribTreeAdaptor
{
	//public GTScribTreeAdaptor(DelFactory df)
	public GTScribTreeAdaptor(ScribAntlrTokens tokens, DelFactory df)
	{
		//super(df);
		super(tokens, df);
	}

	// Duplicated from AssrtScribTreeAdaptor
	// Create a Tree (ScribNode) from a Token
	// N.B. not using AstFactory, construction here is pre adding children (and also here directly record parsed Token, not recreate)
	@Override
	public ScribNode create(Token t)
	{
		// Switching on ScribbleParser "imaginary" token names -- generated from Scribble.g tokens
		// Previously, switched on t.getType(), but arbitrary int constant generation breaks extensibility (e.g., super.create(t))
		ScribNodeBase n;
		switch (t.getText())
		{
			/**
			 *  Create ext node type in place of base
			 *  Parser returns a base token type, we create an ext node type but keep the base token
			 */

			// TODO: integrate with ASSRT variants below?  maybe by un-deprecating reconstructs to make base children configs valid

			/*case "MODULE":
			case "GPROTOHEADER":
				throw new RuntimeException("Deprecated \"" + t.getText() + "\": " + t);

			case "GMSGTRANSFER": //n = new AssrtGMsgTransfer(t); break;
			case "GCONNECT": //n = new AssrtGConnect(t); break;
				throw new RuntimeException("Deprecated \"" + t.getText() + "\": " + t);

				//case AssrtScribbleParser.GCONTINUE: n = new AssrtGContinue(t); break;
			case "GDO": n = new AssrtGDo(t); break;*/

			//case AssrtScribbleParser.GRECURSION: n = new AssrtGRecursion(t); break;


			/**
			 *  Creating explicitly new ext (Assrt) node types
			 *  Parser returns an ext token type, we create the corresponding ext node type
			 */

			/* Simple names "constructed directly" by parser, cf. assrt_varname: t=ID -> ID<AssrtIntVarNameNode>[$t] ; */

			/* Compound names */

			// Non-name (i.e., general) AST nodes
			//case "ASSRT_MODULE": n = new AssrtModule(t); break;
			case "GT_GMIXED": n = new GTGMixed(t); break;

			default:
			{
				n = (ScribNodeBase) super.create(t);  // Assigning "n", but direct return should be the same?  ast decoration pattern should be delegating back to the same df as below
			}
		}
		n.decorateDel(this.df);

		return n;
	}
}
