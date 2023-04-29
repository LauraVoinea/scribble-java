package org.scribble.ext.ea.util;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

public class Tree<T> {

    private final T t;
    private List<Tree<T>> children;

    public Tree(T t, List<Tree<T>> children) {
        this.t = t;
        this.children = Collections.unmodifiableList(new LinkedList<>(children));
    }

    public Tree(T t) {
        this(t, Collections.emptyList());
    }

    public boolean isLeaf() {
        return this.children.size() == 0;
    }

    public T getLab() {
        return this.t;
    }

    public List<Tree<T>> getChildren() {
        return this.children;
    }

    @Override
    public String toString() {
        return toString("");
    }

    protected String toString(String indent) {
        String res = indent + this.t;
        for (Tree<T> c : this.children) {
            res = res + "\n" + c.toString(indent + "  ");
        }
        return res;
    }
}
