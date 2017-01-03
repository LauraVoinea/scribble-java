package org.scribble.parser.ast;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.ast.AstFactoryImpl;
import org.scribble.ast.PayloadElem;
import org.scribble.ast.PayloadElemList;
import org.scribble.ast.name.PayloadElemNameNode;
import org.scribble.ast.name.qualified.DataTypeNode;
import org.scribble.ast.name.qualified.GProtocolNameNode;
import org.scribble.ast.name.simple.AmbigNameNode;
import org.scribble.ast.name.simple.RoleNode;
import org.scribble.ext.f17.ast.name.simple.PayloadVarNode;
import org.scribble.parser.AntlrConstants.AntlrNodeType;
import org.scribble.parser.ScribParser;
import org.scribble.parser.ast.name.AntlrAmbigName;
import org.scribble.parser.ast.name.AntlrQualifiedName;
import org.scribble.parser.ast.name.AntlrSimpleName;
import org.scribble.parser.util.ScribParserUtil;

public class AntlrPayloadElemList
{
	// Cf. AntlrNonRoleArgList
	public static PayloadElemList parsePayloadElemList(ScribParser parser, CommonTree ct)
	{
		// As in AntlrNonRoleArgList, i.e. payloadelem (NonRoleArg) not directly parsed -- cf. rolearg and nonroleparamdecl, which are directly parsed (not consistent), due to amibgious names
		//List<PayloadElem> pes = getPayloadElements(ct).stream().map((pe) -> parsePayloadElem(pe)).collect(Collectors.toList());
		List<PayloadElem<?>> pes = getPayloadElements(ct).stream().map((pe) -> parsePayloadElem(pe)).collect(Collectors.toList());
		return AstFactoryImpl.FACTORY.PayloadElemList(ct, pes);
	}

	//private static PayloadElem parsePayloadElem(CommonTree ct)
	private static PayloadElem<?> parsePayloadElem(CommonTree ct)
	{
		AntlrNodeType type = ScribParserUtil.getAntlrNodeType(ct);
		/*// Parser isn't working to distinguish simple from qualified names (cf. Scribble.g payloadelement)
		if (type == AntlrNodeType.QUALIFIEDNAME)
		{
			DataTypeNameNode dt = AntlrQualifiedName.toDataTypeNameNode(ct);
			return AstFactoryImpl.FACTORY.PayloadElement(dt);
		}
		else if (type == AntlrNodeType.AMBIGUOUSNAME)
		{
			// Similarly to NonRoleArg: cannot syntactically distinguish right now between SimplePayloadTypeNode and ParameterNode
			AmbigNameNode an = AntlrAmbigName.toAmbigNameNode(ct);
			return AstFactoryImpl.FACTORY.PayloadElement(an);
		}*/
		if (type == AntlrNodeType.DELEGATION)
		{
			RoleNode rn = AntlrSimpleName.toRoleNode((CommonTree) ct.getChild(0));
			//GProtocolNameNode gpnn = AntlrQualifiedName.toGProtocolNameNode((CommonTree) ct.getChild(1));  // FIXME:
			GProtocolNameNode gpnn = AntlrQualifiedName.toGProtocolNameNode((CommonTree) ct.getChild(1));
			return AstFactoryImpl.FACTORY.GDelegationElem(ct, gpnn, rn);
			//throw new RuntimeException("Shouldn't get in here: " + ct);
		}

		else if (type == AntlrNodeType.ANNOT_PAYLOADELEMENT)  // FIXME: refactor properly
		{
			PayloadVarNode pvn = AntlrSimpleName.toPayloadVarNode((CommonTree) ct.getChild(0));
			PayloadElemNameNode<?> penn = parsePayloadElemenNameNode((CommonTree) ct.getChild(1));
			return AstFactoryImpl.FACTORY.AnnotUnaryPayloadElem(ct, pvn, penn);
		}

		else if (type == AntlrNodeType.QUALIFIEDNAME)
		{
			return AstFactoryImpl.FACTORY.UnaryPayloadElem(ct, parsePayloadElemenNameNode(ct));
		}
		else
		{
			throw new RuntimeException("Shouldn't get in here: " + ct);
		}
	}

	private static PayloadElemNameNode<?> parsePayloadElemenNameNode(CommonTree ct)
	{
		if (ct.getChildCount() > 1)
		{
			DataTypeNode dt = AntlrQualifiedName.toDataTypeNameNode(ct);
			////return AstFactoryImpl.FACTORY.PayloadElem(dt);
			//return AstFactoryImpl.FACTORY.UnaryPayloadElem(ct, dt);  // return and dt share same ct in this case
			return dt;
		}
		else
		{
			// Similarly to NonRoleArg: cannot syntactically distinguish right now between SimplePayloadTypeNode and ParameterNode
			AmbigNameNode an = AntlrAmbigName.toAmbigNameNode(ct);
			//return AstFactoryImpl.FACTORY.UnaryPayloadElem(ct, an);
			return an;
		}
	}

	public static final List<CommonTree> getPayloadElements(CommonTree ct)
	{
		return (ct.getChildCount() == 0)
				? Collections.emptyList()
				: ScribParserUtil.toCommonTreeList(ct.getChildren());
	}
}
