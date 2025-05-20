package oopsla25.s3;

import oopsla25.TestUtil;
import org.junit.Test;

public class TestS3 {

    @Test
    public void testWFAmbiguous() {
        TestUtil.testBad(TestUtil.BASE_PATH + "\\s3\\bad\\WFAmbiguous.scr");
    }

    @Test
    public void testLabelCapture() {
        TestUtil.testGood(TestUtil.BASE_PATH + "\\s3\\good\\LabelCapture.scr");
    }

    @Test
    public void testRoleCapture() {
        TestUtil.testGood(TestUtil.BASE_PATH + "\\s3\\good\\RoleCapture.scr");
    }

    @Test
    public void testProgress() {
        TestUtil.testGood(TestUtil.BASE_PATH + "\\s3\\good\\Progress.scr");
    }

    @Test
    public void testClearTermination1() {
        TestUtil.testBad(TestUtil.BASE_PATH + "\\s3\\bad\\ClearTermination1.scr");
    }

    @Test
    public void testClearTermination2() {
        TestUtil.testGood(TestUtil.BASE_PATH + "\\s3\\good\\ClearTermination2.scr");
    }

    @Test
    public void testClearTermination3() {
        TestUtil.testGood(TestUtil.BASE_PATH + "\\s3\\good\\ClearTermination3.scr");
    }

    @Test
    public void testBalance() {
        TestUtil.testBad(TestUtil.BASE_PATH + "\\s3\\bad\\Balance.scr");
    }
}
