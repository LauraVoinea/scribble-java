package org.scribble.parser.ast;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.ast.AstFactoryImpl;
import org.scribble.ast.RoleDecl;
import org.scribble.ast.name.simple.RoleNode;
import org.scribble.parser.ScribbleParser;
import org.scribble.parser.ast.name.AntlrSimpleName;

public class AntlrRoleDecl
{
	public static final int NAME_CHILD_INDEX = 0;

	//public static RoleNode parseRoleDecl(AntlrModuleParser parser, CommonTree ct)
	public static RoleDecl parseRoleDecl(ScribbleParser parser, CommonTree ct)
	{
		RoleNode name = AntlrSimpleName.toRoleNode(getNameChild(ct));
		//return new RoleDecl(ct, name);
		//return name;
		return AstFactoryImpl.FACTORY.RoleDecl(name);
	}

	public static CommonTree getNameChild(CommonTree ct)
	{
		return (CommonTree) ct.getChild(NAME_CHILD_INDEX);
	}
}
