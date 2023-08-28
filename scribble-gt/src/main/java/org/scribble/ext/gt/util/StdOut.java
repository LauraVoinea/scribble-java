package org.scribble.ext.gt.util;

import org.scribble.util.Pair;

import java.util.LinkedList;
import java.util.List;
import java.util.function.Consumer;

public class StdOut extends LinkedList<Pair<StdStream, String>> {

    public StdOut() {

    }

    public StdOut(List<Pair<StdStream, String>> c) {
        super(c);
    }

    public boolean add(StdStream s, String x) {
        return add(Pair.of(s, x));
    }

    public boolean addAll(StdOut c) {
        return addAll((List<Pair<StdStream, String>>) c);
    }

    public void println() {
        Consumer<Pair<StdStream, String>> f = x -> {
            if (x.left == StdStream.OUT) {
                ConsoleColors.outPrintln(x.right);
            } else {
                //System.err.println(x.right);
                //System.out.println(ConsoleColors.RED + x.right + ConsoleColors.RESET);

                ConsoleColors.stack.push(ConsoleColors.RED);
                ConsoleColors.outPrintln(x.right);  // !! out, for ordering...
                ConsoleColors.stack.pop();
            }
        };
        forEach(f);
    }
}
