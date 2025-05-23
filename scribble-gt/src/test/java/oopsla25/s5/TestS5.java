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


    /* Table 1 */

    @Test
    public void testCalculator() {
        TestUtil.testGood(TestUtil.BASE_PATH + "\\s5\\good\\table1\\Calculator.scr");
    }

    @Test
    public void testCircuitBreaker() {
        TestUtil.testGood(TestUtil.BASE_PATH + "\\s5\\good\\table1\\CircuitBreaker.scr");
    }

    @Test
    public void testDistributedLogging() {
        TestUtil.testGood(TestUtil.BASE_PATH + "\\s5\\good\\table1\\DistributedLogging.scr");
    }

    @Test
    public void testFibonacci() {
        TestUtil.testGood(TestUtil.BASE_PATH + "\\s5\\good\\table1\\Fibonacci.scr");
    }

    @Test
    public void testSMTP() {
        TestUtil.testGood(TestUtil.BASE_PATH + "\\s5\\good\\table1\\SMTP.scr");
    }

    @Test
    public void testTwoBuyer() {
        TestUtil.testGood(TestUtil.BASE_PATH + "\\s5\\good\\table1\\TwoBuyer.scr");
    }

    @Test
    public void testTravelAgency() {
        TestUtil.testGood(TestUtil.BASE_PATH + "\\s5\\good\\table1\\TravelAgency.scr");
    }

    @Test
    public void testOnlineWallet() {
        TestUtil.testGood(TestUtil.BASE_PATH + "\\s5\\good\\table1\\OnlineWallet.scr");
    }
}
