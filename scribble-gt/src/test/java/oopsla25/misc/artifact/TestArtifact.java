package oopsla25.misc.artifact;

import oopsla25.TestUtil;
import org.junit.Test;

public class TestArtifact {

    @Test
    public void testNMC() {
        TestUtil.testGood(TestUtil.BASE_PATH + "\\misc\\artifact\\good\\nMC.scr");
        TestUtil.testGood(TestUtil.BASE_PATH + "\\misc\\artifact\\good\\Balanced.scr");
        TestUtil.testGood(TestUtil.BASE_PATH + "\\misc\\artifact\\good\\SingleDecision.scr");
        TestUtil.testGood(TestUtil.BASE_PATH + "\\misc\\artifact\\good\\ClearTermination.scr");
        TestUtil.testGood(TestUtil.BASE_PATH + "\\misc\\artifact\\good\\Labels.scr");

        TestUtil.testBad(TestUtil.BASE_PATH + "\\misc\\artifact\\bad\\BalancedBad.scr");
        TestUtil.testBad(TestUtil.BASE_PATH + "\\misc\\artifact\\bad\\SingleDecisionBad.scr");
        TestUtil.testBad(TestUtil.BASE_PATH + "\\misc\\artifact\\bad\\ClearTerminationBad.scr");
        TestUtil.testBad(TestUtil.BASE_PATH + "\\misc\\artifact\\bad\\LabelsBad.scr");
    }
}
