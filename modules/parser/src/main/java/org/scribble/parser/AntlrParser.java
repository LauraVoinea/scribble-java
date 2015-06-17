package org.scribble.parser;

import java.io.IOException;
import java.io.InputStream;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.CommonTree;
import org.scribble.parser.antlr.Scribble2Lexer;
import org.scribble.parser.antlr.Scribble2Parser;
import org.scribble.resources.Resource;


// Resource -> ANTLR CommonTree
// Parses Resources into ANTLR CommonTrees
// Not encapsulated inside ScribbleParser, because ScribbleParser's main function is the higher-level operation of "parsing" CommonTrees into ScribNodes
public class AntlrParser
{
	public AntlrParser()
	{

	}
	
	public CommonTree parseAntlrTree(Resource res)
	{
		try
		{
			String input = new String(readResource(res));
			Scribble2Lexer lex = new Scribble2Lexer(new ANTLRStringStream(input));
			Scribble2Parser parser = new Scribble2Parser(new CommonTokenStream(lex));
			return (CommonTree) parser.module().getTree();
		}
		catch (RecognitionException e)
		{
			throw new RuntimeException(e);
		}
	}

	private static byte[] readResource(Resource res)
	{
		try (InputStream is = res.getInputStream())
		{
			byte[] bs = new byte[is.available()];
			is.read(bs);
			return bs;
		}
		catch (IOException e)
		{
			throw new RuntimeException(e);
		}
	}
}
