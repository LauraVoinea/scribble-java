package oopsla25.s2;

import oopsla25.TestUtil;
import org.junit.Test;
import org.scribble.ext.gt.cli.GTCommandLine2;

public class TestS2 {

    @Test
    public void testTwoPartyChoice() {
        TestUtil.testGood(TestUtil.BASE_PATH + "\\s2\\good\\TwoPartyChoice.scr");
    }

    @Test
    public void testTwoPartyChoiceNonDirected() {
        TestUtil.testBad(TestUtil.BASE_PATH + "\\s2\\bad\\TwoPartyChoiceNonDirected.scr");
    }

    @Test
    public void testTwoPartyMC() {
        TestUtil.testGood(TestUtil.BASE_PATH + "\\s2\\good\\TwoPartyMC.scr");
    }

    @Test
    public void testTimeout() {
        TestUtil.testGood(TestUtil.BASE_PATH + "\\s2\\good\\Timeout.scr");
    }
}
