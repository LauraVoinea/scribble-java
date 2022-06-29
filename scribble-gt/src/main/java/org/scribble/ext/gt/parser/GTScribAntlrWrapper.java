package org.scribble.ext.gt.parser;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.Lexer;
import org.antlr.runtime.Parser;
import org.scribble.del.DelFactory;
import org.scribble.parser.ScribAntlrTokens;
import org.scribble.parser.ScribAntlrWrapper;
import org.scribble.parser.antlr.GTScribbleLexer;
import org.scribble.parser.antlr.GTScribbleParser;

public class GTScribAntlrWrapper extends ScribAntlrWrapper
{
	public GTScribAntlrWrapper(DelFactory df)
	{
		super(df);
	}

	@Override
	protected String[] getTokenNames()
	{
		return GTScribbleParser.tokenNames;
	}
	
	@Override
	public Lexer newScribbleLexer(ANTLRStringStream ss)
	{
		return new GTScribbleLexer(ss);
	}
	
	@Override
	public Parser newScribbleParser(CommonTokenStream ts)
	{
		return new GTScribbleParser(ts);
	}

	@Override
	protected GTScribTreeAdaptor newAdaptor(ScribAntlrTokens tokens, DelFactory df)
	{
		return new GTScribTreeAdaptor(tokens, df);
	}
}
