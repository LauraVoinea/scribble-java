package org.scribble2.model;

import org.scribble2.model.name.qualified.MemberNameNode;
import org.scribble2.sesstype.kind.SigKind;
import org.scribble2.sesstype.name.MemberName;

public class MessageSigDecl extends NonProtocolDecl<SigKind> //AbstractNode implements ModuleMember //implements NameDeclaration
{
	// FIXME: need to replace ParameterNode by a signature member node
	//public MessageSigDecl(String schema, String extName, String source, SimpleMessageSignatureNameNode alias)
	public MessageSigDecl(String schema, String extName, String source, MemberNameNode<? extends MemberName<SigKind>, SigKind> alias)
	{
		super(schema, extName, source, alias);
	}

	/*@Override
	public PayloadTypeDecl disambiguate(PayloadTypeOrParameterDisambiguator disamb) throws ScribbleException
	{
		PayloadTypeDecl visited = (PayloadTypeDecl) super.disambiguate(disamb);
		disamb.addPayloadType(visited);
		return visited;
	}*/

	/*@Override
	public PayloadTypeDecl visitChildren(NodeVisitor nv) throws ScribbleException
	{
		SimplePayloadTypeNode alias = (SimplePayloadTypeNode) visitChild(this.alias, nv);
		return new PayloadTypeDecl(this.ct, this.schema, this.extName, this.source, alias);
	}*/

	/*@Override
	protected DataTypeDecl reconstruct(CommonTree ct, String schema, String extName, String source, PrimitiveNameNode alias)
	{
		return new MessageSignatureDecl(ct, schema, extName, source, (SimpleMessageSignatureNameNode) alias);
	}*/

	@Override
	public String toString()
	{
		return Constants.SIG_KW + " <" + this.schema + "> " + this.extName
				+ " " + Constants.FROM_KW + " " + this.source + " "
				+ Constants.AS_KW + " " + this.alias + ";";
	}

	@Override
	protected MessageSigDecl copy()
	{
		return new MessageSigDecl(this.schema, this.extName, this.source, this.alias);
	}

	/*public PayloadType getFullPayloadTypeName()
	{
		ModuleName fullmodname = AntlrModule.getFullModuleName(AntlrPayloadTypeDecl.getModuleParent(this.ct));
		return new PayloadType(fullmodname + "." + this.alias.toName());
	}*/

	// FIXME: compound full name for declared sigs
	/*public Parameter getFullMessageSignatureParameterName()
	{
		ModuleName fullmodname = AntlrModule.getFullModuleName(AntlrPayloadTypeDecl.getModuleParent(this.ct));
		return new Parameter(Kind.SIG, fullmodname + "." + this.alias.toName());
	}*/

	/*public MessageSignatureName getFullMessageSignatureName()
	{
		ModuleName fullmodname = AntlrModule.getFullModuleName(AntlrPayloadTypeDecl.getModuleParent(this.ct));
		return new MessageSignatureName(fullmodname, this.alias.toName().toString());
	}

	@Override
	public MessageSignatureName getAliasName()
	{
		//return ((ParameterNode) this.alias).toName();
		return ((SimpleMessageSignatureNameNode) this.alias).toName();
	}*/
}
