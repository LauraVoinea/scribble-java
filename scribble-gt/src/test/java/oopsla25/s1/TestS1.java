package oopsla25.s1;

import oopsla25.TestUtil;
import org.junit.Test;
import org.scribble.ext.gt.cli.GTCommandLine2;

public class TestS1 {

    @Test
    public void testIntro() {
        //GTCommandLine2.main(new String[]{TestUtil.BASE_PATH + "\\s1\\good\\Intro.scr"});
        TestUtil.testGood(TestUtil.BASE_PATH + "\\s1\\good\\Intro.scr");
    }
}
