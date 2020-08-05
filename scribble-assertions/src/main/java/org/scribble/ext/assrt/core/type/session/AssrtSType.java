package org.scribble.ext.assrt.core.type.session;

import java.util.Map;
import java.util.function.Function;
import java.util.stream.Stream;

import org.scribble.core.type.kind.ProtoKind;
import org.scribble.core.type.name.DataName;
import org.scribble.core.type.session.SType;
import org.scribble.ext.assrt.core.type.name.AssrtVar;

public interface AssrtSType<K extends ProtoKind, B extends AssrtSType<K, B>>
		extends SType<K, NoSeq<K>>
{
	<T> Stream<T> assrtCoreGather(Function<AssrtSType<K, B>, Stream<T>> f);

	// Return "additional" env items -- i.e., ctxt not included in result
	Map<AssrtVar, DataName> getBoundSortEnv(Map<AssrtVar, DataName> ctxt);
}
