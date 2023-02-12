package org.scribble.core.visit.local;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.util.ScribException;

public class UnboundedRecursionException extends ScribException {

    public UnboundedRecursionException(CommonTree blame, String arg0) {
        super(blame, arg0);
    }

    public UnboundedRecursionException() {
        super();
        // TODO Auto-generated constructor stub
    }

    public UnboundedRecursionException(String arg0) {
        super(arg0);
        // TODO Auto-generated constructor stub
    }

    public UnboundedRecursionException(Throwable arg0) {
        super(arg0);
        // TODO Auto-generated constructor stub
    }

    public UnboundedRecursionException(String arg0, Throwable arg1) {
        super(arg0, arg1);
        // TODO Auto-generated constructor stub
    }

    public UnboundedRecursionException(String arg0, Throwable arg1, boolean arg2,
                                       boolean arg3) {
        super(arg0, arg1, arg2, arg3);
        // TODO Auto-generated constructor stub
    }
}
