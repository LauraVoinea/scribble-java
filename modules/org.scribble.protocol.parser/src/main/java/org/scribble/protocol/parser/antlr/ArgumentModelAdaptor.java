/*
 * Copyright 2009-11 www.scribble.org
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package org.scribble.protocol.parser.antlr;

import org.antlr.runtime.CommonToken;
import org.scribble.protocol.model.Argument;
import org.scribble.protocol.model.MessageSignature;

/**
 * This class provides the model adapter for the 'messageSignature' parser rule.
 *
 */
public class ArgumentModelAdaptor implements ModelAdaptor {

	/**
	 * {@inheritDoc}
	 */
	public Object createModelObject(ParserContext context) {
		
		Argument ret=new Argument();
		
		if (context.peek() instanceof MessageSignature) {
			ret.setMessageSignature((MessageSignature)context.pop());
		} else {
			ret.setParameter(((CommonToken)context.pop()).getText());
		}
		
		if (context.peek() instanceof CommonToken &&
				((CommonToken)context.peek()).getText().equals("as")) {
			context.pop(); // consume 'as'
			
			ret.setAlias(((CommonToken)context.pop()).getText());
		}
		
		context.push(ret);
			
		return ret;
	}

}
