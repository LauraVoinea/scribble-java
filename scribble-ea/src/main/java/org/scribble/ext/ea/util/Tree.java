package org.scribble.ext.ea.util;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

public class Tree<T> {

    private final T val;
    private List<Tree<T>> children;

    // TODO deprecate
    public Tree(T val, Tree<T>... children) {
        this(val, Arrays.asList(children));
    }

    // TODO make protected
    public Tree(T val, List<Tree<T>> children) {
        this.val = val;
        this.children = Collections.unmodifiableList(new LinkedList<>(children));
    }

    public <T> Tree<T> of(T val, Tree<T>... children) {
        return new Tree(val, Arrays.asList(children));
    }

    public <T> Tree<T> of(T val, List<Tree<T>> children) {
        return new Tree(val, children);
    }

    /*public Tree(T t) {
        this(t, Collections.emptyList());
    }*/

    public boolean isLeaf() {
        return this.children.size() == 0;
    }

    public T getLab() {
        return this.val;
    }

    public List<Tree<T>> getChildren() {
        return this.children;
    }

    @Override
    public String toString() {
        return toString("");
    }

    public String toString(String indent) {
        String res = indent + this.val;
        for (Tree<T> c : this.children) {
            res = res + "\n" + c.toString(indent + "  ");
        }
        return res;
    }
}
