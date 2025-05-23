package oopsla25;

import org.junit.Assert;
import org.scribble.ext.gt.cli.GTCommandLine2;
//import static org.junit.Assert.assertThrows;

public class TestUtil {

    public static final String BASE_PATH = "C:\\Users\\Raymond\\winroot\\home\\eey335\\code\\java\\intellij\\git\\github.com\\rhu1-scribble-core-gt\\scribble-java\\scribble-gt\\src\\test\\java\\oopsla25";

    public static void testGood(String scribFile) {
        GTCommandLine2.main(new String[]{scribFile});
    }

    // TODO GTException
    public static void testBad(String scribFile) {
        // cf. Assertion.assertThrows
        try {
            GTCommandLine2.main(new String[]{scribFile});
        } catch (RuntimeException x) {
            return;
        }
        Assert.fail("Expected exception.");
    }
}
