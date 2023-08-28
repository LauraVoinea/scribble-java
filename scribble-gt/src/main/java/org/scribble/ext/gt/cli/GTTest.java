package org.scribble.ext.gt.cli;

import org.scribble.core.type.name.GProtoName;
import org.scribble.ext.gt.core.model.GTCorrespondence;
import org.scribble.ext.gt.core.type.session.global.GTGType;
import org.scribble.ext.gt.util.*;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.*;
import java.util.function.BiFunction;

public class GTTest {

    public static void main(String[] args) {
        //GTCommandLine.main(new String[]{"-v", "-fair", "C:\\Users\\Raymond\\winroot\\home\\eey335\\code\\java\\intellij\\git\\github.com\\rhu1-scribble-core-gt\\scribble-java\\scribble-test\\src\\test\\scrib\\tmp\\Test.scr"});

        //Optional<Exception> res = GTCommandLine.mainTest(new String[]{"-v", "-fair", "-nocorr", "-inline", "module GTTest; global protocol P(role A, role B) { 1() from A to B; }"});
        //runTest("module GTTest; global protocol P(role A, role B) { mixed { 1() from A to B; 3() from B to A; } () or A->B () { 2() from B to A; } }");

        staticTest();
    }

    static void staticTest() {

        testInitialWellSet().println();

        // initial awareness
        testSingleDecision().println();
        testClearTermination().println();
    }

    static void runtimeTest() {

    }

    // -nocorr  // TODO corr -- factor out preservation of projection, and preservation of correspondence with full safety properties
    static Optional<Exception> runTest(StdOut out, int i, String proto) {
        out.add(StdStream.OUT, "Testing -nocorr (" + i + "): global protocol " + proto);
        return GTCommandLine.mainTest(new String[]{"-fair", "-nocorr", "-inline", "module Test; global protocol " + proto});
    }

    static Optional<Exception> runInitialWellSetTest(StdOut out, int i, String proto) {
        out.add(StdStream.OUT, "Testing initial well-set (" + i + "): global protocol " + proto);
        GTCommandLine cl = GTCommandLine.init(new String[]{"-fair", "-nocorr", "-inline", "module Test; global protocol " + proto});
        Map<GProtoName, GTGType> translated = GTCommandLine.getTranslated(cl);
        return GTCommandLine.checkInitialWellSet(translated.values().iterator().next());
    }

    static Optional<Exception> runSingleDecision(StdOut out, int i, String proto) {
        out.add(StdStream.OUT, "Testing single-decision (" + i + "): global protocol " + proto);
        GTCommandLine cl = GTCommandLine.init(new String[]{"-fair", "-nocorr", "-inline", "module Test; global protocol " + proto});
        Map<GProtoName, GTGType> translated = GTCommandLine.getTranslated(cl);
        return GTCommandLine.checkSingleDecision(translated.values().iterator().next());
    }

    static Optional<Exception> runClearTermination(StdOut out, int i, String proto) {
        out.add(StdStream.OUT, "Testing clear termination (" + i + "): global protocol " + proto);
        GTCommandLine cl = GTCommandLine.init(new String[]{"-fair", "-nocorr", "-inline", "module Test; global protocol " + proto});
        Map<GProtoName, GTGType> translated = GTCommandLine.getTranslated(cl);
        return GTCommandLine.checkClearTermination(translated.values().iterator().next());
    }

    static Optional<Exception> runProjection(StdOut out, int i, String proto) {
        out.add(StdStream.OUT, "Testing static projection (" + i + "): global protocol " + proto);
        GTCommandLine cl = GTCommandLine.init(new String[]{"-fair", "-nocorr", "-inline", "module Test; global protocol " + proto});
        Map<GProtoName, GTGType> translated = GTCommandLine.getTranslated(cl);
        Either<Exception, GTCorrespondence> res = GTCommandLine.checkProjection(translated.values().iterator().next());
        return res.isLeft() ? Optional.of(res.getLeft()) : Optional.empty();
    }


    /* ... */

    static StdOut runTests(List<String> protos,
                           TriFunction<StdOut, Integer, String, Optional<Exception>> run,
                           BiFunction<StdOut, Optional<Exception>, Boolean> check,
                           String title) {

        StdOut out = new StdOut();
        out.add(StdStream.OUT, "\n---\nTesting " + title);
        out.add(StdStream.OUT, "");

        Map<Integer, Boolean> summ = new HashMap<>();
        int i = 1;
        for (String proto : protos) {
            //Optional<Exception> res = runTest(i, proto);
            //Optional<Exception> res = runInitialWellSetTest(i, proto);
            Optional<Exception> res = run.apply(out, i, proto);
            /*if (res.isPresent()) {
                //res.get().printStackTrace();  // stderr
                //res.get().printStackTrace(System.out);
                StringWriter sw = new StringWriter();
                PrintWriter pw = new PrintWriter(sw);
                res.get().printStackTrace(pw);
                out.add(StdStream.ERR, sw.toString());
                summ.put(i, false);
            } else {
                summ.put(i, true);
            }*/
            summ.put(i, check.apply(out, res));
            i++;
        }
        //System.out.println("\nSummary good tests: ");
        out.add(StdStream.OUT, "\nSummary " + title + ": ");
        int count = 0;
        for (Map.Entry<Integer, Boolean> x : summ.entrySet()) {
            boolean v = x.getValue();
            if (v) {
                count++;
            }
            out.add(StdStream.OUT, x.getKey() + " "
                    + (v ? ConsoleColors.GREEN : ConsoleColors.RED)
                    + v
                    + ConsoleColors.RESET);
        }
        //System.out.println("Passed: " + count + "/" + summ.size());
        int total = summ.size();
        out.add(StdStream.OUT,
                (count == total ? ConsoleColors.GREEN : ConsoleColors.RED)
                        + "Passed: "
                        + count + "/" + total
                        + ConsoleColors.RESET);
        return out;
    }

    static StdOut runGoodTests(
            List<String> protos, TriFunction<StdOut, Integer, String, Optional<Exception>> run, String title) {
        /*StdOut out = new StdOut();
        out.add(StdStream.OUT, "");
        Map<Integer, Boolean> summ = new HashMap<>();
        int i = 1;
        for (String proto : protos) {
            //Optional<Exception> res = runTest(i, proto);
            //Optional<Exception> res = runInitialWellSetTest(i, proto);
            Optional<Exception> res = run.apply(i, proto);
            if (res.isPresent()) {
                //res.get().printStackTrace();  // stderr
                //res.get().printStackTrace(System.out);
                StringWriter sw = new StringWriter();
                PrintWriter pw = new PrintWriter(sw);
                res.get().printStackTrace(pw);
                out.add(StdStream.ERR, sw.toString());
                summ.put(i, false);
            } else {
                summ.put(i, true);
            }
            i++;
        }
        //System.out.println("\nSummary good tests: ");
        out.add(StdStream.OUT, "\nSummary good tests: ");
        int count = 0;
        for (Map.Entry<Integer, Boolean> x : summ.entrySet()) {
            boolean v = x.getValue();
            if (v) {
                count++;
            }
            System.out.println(x.getKey() + " " + v);
        }
        //System.out.println("Passed: " + count + "/" + summ.size());
        out.add(StdStream.OUT, "Passed: " + count + "/" + summ.size());
        return out;*/
        BiFunction<StdOut, Optional<Exception>, Boolean> f = (out, x) -> {
            if (x.isPresent()) {
                out.add(StdStream.ERR, toTraceString(x.get()));
                return false;
            } else {
                return true;
            }
        };
        return runTests(protos, run, f, title);
    }

    static StdOut runBadTests(
            List<String> protos, TriFunction<StdOut, Integer, String, Optional<Exception>> run, String title) {
        /*System.out.println();
        Map<Integer, Boolean> summ = new HashMap<>();
        int i = 1;
        for (String proto : protos) {
            //Optional<Exception> res = runInitialWellSetTest(i, proto);
            Optional<Exception> res = run.apply(i, proto);
            if (res.isEmpty()) {
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
        System.out.println("Passed: " + count + "/" + summ.size());*/
        BiFunction<StdOut, Optional<Exception>, Boolean> f = (out, x) -> {
            if (x.isEmpty()) {
                out.add(StdStream.ERR, "Missing error...\n");
                return false;
            } else {
                return true;
            }
        };
        return runTests(protos, run, f, title);
    }

    /* ... */

    static StdOut testInitialWellSet() {
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

        List<String> bad = new LinkedList<>();

        //// ...currently committed roles ignored by translator -- TODO fully remove run-time syntax from Scribble? (cf. wiggly, mixed-active)
        ////bad.add("P(role A, role B) { mixed { l1() from A to B; } (A) or A->B () { r1() from B to A; } }");
        String title = "initial well-set";
        StdOut out = runGoodTests(good, GTTest::runSingleDecision, title + " (good)");
        out.addAll(runBadTests(bad, GTTest::runSingleDecision, title + " (bad)"));
        return out;
    }

    static StdOut testSingleDecision() {
        List<String> good = new LinkedList<>();
        List<String> bad = new LinkedList<>();

        /*good.add("P(role A, role B, role C, role D) {"  // C indifferent XXX ?
                + "mixed { l1() from A to B; l2() from A to C; l3() from C to D; } () or A->B ()"
                + "      { r1() from B to A; r2() from C to A; l3() from C to D; } }");*/

        good.add("G0(role A, role B) { l1() from A to B;  r1() from A to B; }");

        good.add("G0a(role A, role B) { "
                + "mixed { l1() from A to B; } () or A->B () "  // XXX well-term
                + "      { r1() from B to A; } }");
        bad.add("B0(role A, role B) { "
                + "mixed { l1() from A to B; } () or A->B () "  // XXX well-term
                + "      { r1() from A to B; } }");

        good.add("G1(role A, role B, role C) { "
                + "mixed { l1() from A to B; } () or A->B () "  // XXX well-term
                + "      { r1() from B to A; r2() from B to C; } }");
        bad.add("B1(role A, role B, role C) { "
                + "mixed { l1() from A to B; l2() from A to C; } () or A->B () "  // XXX well-term
                + "      { r1() from B to A; } }");

        good.add("G1a(role A, role B, role C) { "
                + "mixed { l1() from A to B; l2() from B to A; l3() from B to C; } () or A->B () "
                + "      { r1() from B to A; r2() from A to C; } }");
        bad.add("B1a(role A, role B, role C) { "
                + "mixed { l1() from A to B; l2() from B to A; l3() from B to C; } () or A->B () "
                + "      { r1() from B to C; } }");

        good.add("G2(role A, role B, role C, role D) {"  // C indifferent
                + "mixed { l1() from A to B; l2() from A to C; l3() from C to D; } () or A->B ()"
                + "      { r1() from B to A; l2() from A to C; l3() from C to D; } }");
        good.add("G3(role A, role B, role C, role D) {"  // C indifferent
                + "mixed { l1() from A to B; l2() from C to A; l3() from C to D; } () or A->B ()"
                + "      { r1() from B to A; l2() from C to A; l3() from C to D; } }");
        bad.add("B2(role A, role B, role C, role D) {"  // C not indifferent and not single-dec
                + "mixed { l1() from A to B; l2() from A to C; l3() from C to D; } () or A->B ()"
                + "      { r1() from B to A; r2() from C to A; r3() from C to D; } }");

        good.add("G4(role A, role B) {"
                + "mixed { l1() from A to B; mixed { l2() from B to A; l3() from A to B; } () or B->A () "
                + "                                { l4() from A to B; l5() from B to A; }"
                + "      } () or A->B ()"
                + "      { r1() from B to A; }"
                + "}");

        good.add("G5(role A, role B, role C) {"
                + "mixed { l1() from A to B; } () or A->B ()"   // XXX well-term
                + "      { r1() from B to A; r2() from B to C; mixed { r3() from B to C; } () or B->C () "
                + "                                                  { r4() from C to B; } } }");
        bad.add("B5(role A, role B, role C) {"
                + "mixed { l1() from A to B; } () or A->B ()"   // XXX well-term
                + "      { r1() from B to A; mixed { r2() from B to C; } () or B->C () "
                + "                                { r3() from C to B; } } }");
        bad.add("G5(role A, role B, role C) {"
                + "mixed { l1() from A to B; } () or A->B ()"   // XXX well-term
                + "      { r1() from B to A; mixed { r2() from B to C; } () or B->C () "
                + "                                { r3() from C to B; r4() from B to C; } } }");

        String title = "initial single decision";
        StdOut out = runGoodTests(good, GTTest::runSingleDecision, title + " (good)");
        out.addAll(runBadTests(bad, GTTest::runSingleDecision, title + " (bad)"));
        return out;
    }

    static StdOut testClearTermination() {

        List<String> good = new LinkedList<>();
        List<String> bad = new LinkedList<>();

        /*good.add("G1(role A, role B) {"
                + "mixed { l1() from A to B; } () or A->B ()"  // Bad -- testing
                + "      { r1() from B to A; } }");*/
        good.add("G1(role A, role B) {"
                + "mixed { l1() from A to B; l2() from B to A; } () or A->B ()"
                + "      { r1() from B to A; } }");
        good.add("G2(role A, role B, role C) {"
                + "mixed { l1() from A to B; l2() from B to A; l3() from B to C; } () or A->B ()"
                + "      { r1() from B to A; r2() from B to C; } }");
        good.add("G3(role A, role B, role C) {"
                + "mixed { l1() from A to B; l2() from B to A; l3() from A to C; } () or A->B ()"
                + "      { r1() from B to A; r2() from B to C; } }");
        good.add("G4(role A, role B, role C) {"
                + "mixed { l1() from A to B; l2() from C to A; l3() from B to A; l4() from B to C; } () or A->B ()"
                + "      { r1() from B to A; r2() from B to C; } }");

        /*bad.add("G1(role A, role B) {"
                + "mixed { l1() from A to B; l2() from B to A; } () or A->B ()"  // Good -- testing
                + "      { r1() from B to A; } }");*/
        bad.add("B1(role A, role B, role C) {"
                + "mixed { l1() from A to B; } () or A->B ()"
                + "      { r1() from B to A; } }");
        bad.add("B2(role A, role B, role C) {"
                + "mixed { l1() from A to B; l2() from A to C; } () or A->B ()"
                + "      { r1() from B to A; r2() from B to C; } }");
        bad.add("B3(role A, role B, role C) {"
                + "mixed { l1() from A to B; l2() from A to C; l3() from B to A; } () or A->B ()"
                + "      { r1() from B to A; r2() from B to C; } }");
        bad.add("B4(role A, role B, role C) {"
                + "mixed { l1() from A to B; l2() from B to C; } () or A->B ()"
                + "      { r1() from B to A; r2() from B to C; } }");
        bad.add("B5(role A, role B, role C) {"
                + "mixed { l1() from A to B; l2() from C to A; l3() from B to C; } () or A->B ()"
                + "      { r1() from B to A; r2() from B to C; } }");

        good.add("G5(role A, role B) {"
                + "mixed { l1() from A to B; mixed { l2() from B to A; l3() from A to B; } () or B->A () "
                + "                                { l4() from A to B; l5() from B to A; }"
                + "      } () or A->B ()"
                + "      { r1() from B to A; }"
                + "}");

        bad.add("B6(role A, role B) {"
                + "mixed { l1() from A to B; mixed { l2() from B to A; l3() from A to B; } () or B->A () "
                + "                                { l4() from A to B; }"
                + "      } () or A->B ()"
                + "      { r1() from B to A; }"
                + "}");
        bad.add("B7(role A, role B) {"
                + "mixed { l1() from A to B; mixed { l2() from B to A; } () or B->A () "
                + "                                { l4() from A to B; l5() from B to A; }"
                + "      } () or A->B ()"
                + "      { r1() from B to A; }"
                + "}");

        String title = "initial clear termination";
        StdOut out = runGoodTests(good, GTTest::runClearTermination, title + " (good)");
        out.addAll(runBadTests(bad, GTTest::runClearTermination, title + " (bad)"));
        return out;
    }


    /* ... */

    // TODO move
    public static String toTraceString(Exception x) {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        x.printStackTrace(pw);
        return sw.toString();
    }
}
