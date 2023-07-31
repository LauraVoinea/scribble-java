package org.scribble.ext.gt.cli;

public class GTTest {

    public static void main(String[] args) {
        //GTCommandLine.main(new String[]{"-v", "-fair", "C:\\Users\\Raymond\\winroot\\home\\eey335\\code\\java\\intellij\\git\\github.com\\rhu1-scribble-core-gt\\scribble-java\\scribble-test\\src\\test\\scrib\\tmp\\Test.scr"});
        GTCommandLine.main(new String[]{"-v", "-fair", "-inline", "module GTTest; global protocol P(role A, role B) { 1() from A to B; }"});
    }
}
