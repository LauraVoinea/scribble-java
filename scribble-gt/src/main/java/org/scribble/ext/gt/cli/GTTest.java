package org.scribble.ext.gt.cli;

import java.util.Optional;

public class GTTest {

    public static void main(String[] args) {
        //GTCommandLine.main(new String[]{"-v", "-fair", "C:\\Users\\Raymond\\winroot\\home\\eey335\\code\\java\\intellij\\git\\github.com\\rhu1-scribble-core-gt\\scribble-java\\scribble-test\\src\\test\\scrib\\tmp\\Test.scr"});

        //Optional<Exception> res = GTCommandLine.mainTest(new String[]{"-v", "-fair", "-nocorr", "-inline", "module GTTest; global protocol P(role A, role B) { 1() from A to B; }"});
        Optional<Exception> res = GTCommandLine.mainTest(new String[]{"-v", "-fair", "-nocorr", "-inline", "module GTTest; global protocol P(role A, role B) { mixed { 1() from A to B; 3() from B to A; } () or A->B () { 2() from B to A; } }"});
        System.out.print(res);
    }
}
