package org.scribble.ext.gt.cli;

import org.scribble.core.type.name.GProtoName;
import org.scribble.core.type.name.Op;
import org.scribble.ext.gt.core.type.session.global.GTGType;

import java.util.*;
import java.util.function.Supplier;

public class GTTest {

    public static void main(String[] args) {
        //GTCommandLine.main(new String[]{"-v", "-fair", "C:\\Users\\Raymond\\winroot\\home\\eey335\\code\\java\\intellij\\git\\github.com\\rhu1-scribble-core-gt\\scribble-java\\scribble-test\\src\\test\\scrib\\tmp\\Test.scr"});

        //Optional<Exception> res = GTCommandLine.mainTest(new String[]{"-v", "-fair", "-nocorr", "-inline", "module GTTest; global protocol P(role A, role B) { 1() from A to B; }"});
        //runTest("module GTTest; global protocol P(role A, role B) { mixed { 1() from A to B; 3() from B to A; } () or A->B () { 2() from B to A; } }");

        testInitialWellSet();
    }

    static Optional<Exception> runTest(int i, String proto) {
        System.out.println("Running test " + i + ": global protocol " + proto);
        return GTCommandLine.mainTest(new String[]{"-fair", "-nocorr", "-inline", "module Test; global protocol " + proto});
    }

    static Optional<Exception> runInitialWellSetTest(int i, String proto) {
        System.out.println("Running test " + i + ": global protocol " + proto);
        GTCommandLine cl = GTCommandLine.init(new String[]{"-fair", "-nocorr", "-inline", "module Test; global protocol " + proto});
        Map<GProtoName, GTGType> translated = GTCommandLine.getTranslated(cl);
        return GTCommandLine.checkInitialWellSet(translated.values().iterator().next());
    }

    static void runGoodTests(List<String> protos) {
        Map<Integer, Boolean> summ = new HashMap<>();
        int i = 1;
        for (String proto : protos) {
            //Optional<Exception> res = runTest(i, proto);
            Optional<Exception> res = runInitialWellSetTest(i, proto);
            if (res.isPresent()) {
                res.get().printStackTrace();
                summ.put(i, false);
            } else {
                summ.put(i, true);
            }
            i++;
        }
        System.out.println("\nSummary good tests: ");
        int count = 0;
        for (Map.Entry<Integer, Boolean> x : summ.entrySet()) {
            boolean v = x.getValue();
            if (v) {
                count++;
            }
            System.out.println(x.getKey() + " " + v);
        }
        System.out.println("Passed: " + count + "/" + summ.size());
    }

    static void runBadTests(List<String> protos) {
        Map<Integer, Boolean> summ = new HashMap<>();
        int i = 1;
        for (String proto : protos) {
            Optional<Exception> res = runInitialWellSetTest(i, proto);
            if (!res.isPresent()) {
                System.out.println("Missing error.");
                summ.put(i, false);
            } else {
                summ.put(i, true);
            }
            i++;
        }
        System.out.println("\nSummary bad tests: ");
        int count = 0;
        for (Map.Entry<Integer, Boolean> x : summ.entrySet()) {
            boolean v = x.getValue();
            if (v) {
                count++;
            }
            System.out.println(x.getKey() + " " + v);
        }
        System.out.println("Passed: " + count + "/" + summ.size());
    }

    static void testInitialWellSet() {
        List<String> good = new LinkedList<>();
        good.add("P(role A, role B) { l1() from A to B; }");
        good.add("P(role A, role B) { mixed { l1() from A to B; l2() from B to A; } () or A->B () { r1() from B to A; } }");
        good.add("P(role A, role B) { "
                + "mixed {"
                + "  choice at A { l1() from A to B; l2() from B to A; } "
                + "           or { l3() from A to B; l4() from B to A; }"
                + "} () or A->B () {"
                + "  r1() from B to A; }"
                + "}");
        runGoodTests(good);

        List<String> bad = new LinkedList<>();
        bad.add("P(role A, role B) { mixed { l1() from A to B; } () or A->B () { r1() from B to A; } }");
        runBadTests(bad);
    }

    static void testSingleDecision() {

    }

    static void testClearTermination() {

    }
}
