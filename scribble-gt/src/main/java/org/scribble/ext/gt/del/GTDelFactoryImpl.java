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
package org.scribble.ext.gt.del;

import org.scribble.del.DelFactoryImpl;
import org.scribble.ext.gt.ast.global.GTGMixed;
import org.scribble.ext.gt.del.global.GTGMixedDel;


public class GTDelFactoryImpl extends DelFactoryImpl implements GTDelFactory
{
	/**
	 * Existing (base) node classes with new dels
   * (Instantiating existing node classes with new dels)
	 */


	/* General and globals */

	/* Locals */


	/**
	 * New (Assrt) node types  
	 */
	
	/* General and globals */

	@Override
	public void GTGMixed(GTGMixed n)
	{
		//setDel(n, createDefaultDel());
		setDel(n, new GTGMixedDel());
	}
	

	/**
	 * New (Assrt) node types
	 * (Explicitly creating new Assrt nodes)
	 */

	/* General and globals */
}
