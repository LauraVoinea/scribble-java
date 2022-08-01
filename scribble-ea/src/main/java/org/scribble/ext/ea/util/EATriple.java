/*
 * Copyright 2008 The Scribble Authors
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
 */
package org.scribble.ext.ea.util;

public class EATriple<T1, T2, T3>
{
	public final T1 left;
	public final T2 mid;
	public final T3 right;

	public EATriple(T1 t1, T2 t2, T3 t3)
	{
		this.left = t1;
		this.mid = t2;
		this.right = t3;
	}
	
	@Override
	public int hashCode()
	{
		int hash = 13;
		hash = 31 * hash + this.left.hashCode();
		hash = 31 * hash + this.mid.hashCode();
		hash = 31 * hash + this.right.hashCode();
		return hash;
	}
	
	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof EATriple))
		{
			return false;
		}
		EATriple<?, ?, ?> them = (EATriple<?, ?, ?>) o;
		return this.left.equals(them.left)
				&& this.mid.equals(them.mid)
				&& this.right.equals(them.right);
	}
}
