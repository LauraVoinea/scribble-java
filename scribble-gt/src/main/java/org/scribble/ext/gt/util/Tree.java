package org.scribble.ext.gt.util;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

public class Tree<T> {

    public final T val;
    public final List<Tree<T>> children;

    /*protected Tree(T val, Tree<T>... children) {
        this(val, Arrays.asList(children));
    }*/

    protected Tree(T val, List<Tree<T>> children) {
        this.val = val;
        this.children = Collections.unmodifiableList(new LinkedList<>(children));
    }

    public static <T> Tree<T> of(T val, Tree<T>... children) {
        return new Tree<>(val, Arrays.asList(children));
    }

    public static <T> Tree<T> of(T val, List<Tree<T>> children) {
        return new Tree<>(val, children);
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
