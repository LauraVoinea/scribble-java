package oopsla25.s5;

import oopsla25.TestUtil;
import org.junit.Test;

public class TestS5 {

    @Test
    public void testFailure() {
        TestUtil.testGood(TestUtil.BASE_PATH + "\\s5\\good\\Failure.scr");
    }

    @Test
    public void testInterr() {
        TestUtil.testGood(TestUtil.BASE_PATH + "\\s5\\good\\Interr.scr");
    }

    @Test
    public void testAMQP() {
        TestUtil.testGood(TestUtil.BASE_PATH + "\\s5\\good\\AMQP.scr");
    }

    @Test
    public void testAMQP_long() {
        TestUtil.testGood(TestUtil.BASE_PATH + "\\s5\\good\\AMQP_long.scr");
    }
}
