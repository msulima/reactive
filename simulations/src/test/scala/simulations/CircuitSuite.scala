package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    testOr(in1, in2, out)
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    testOr(in1, in2, out)
  }

  def testOr(in1: Wire, in2: Wire, out: Wire) {
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")
  }

  test("simple demux") {
    val in, c, out1, out2 = new Wire
    demux(in, List(c), List(out1, out2))
    run

    def checkOutputs(exp1: Boolean, exp2: Boolean, round: Int) {
      assert(out1.getSignal === exp1, s"out1 $round")
      assert(out2.getSignal === exp2, s"out2 $round")
    }

    checkOutputs(false, false, 1)

    in.setSignal(true);
    run
    checkOutputs(true, false, 2)

    c.setSignal(true);
    run
    checkOutputs(false, true, 3)

    in.setSignal(false);
    run
    checkOutputs(false, false, 4)
  }

  implicit def asBoolean(int: Int) = int != 0
  implicit def asBoolean(tuple: (Int, Int, Int)): (Boolean, Boolean, Boolean) =
    	(tuple._1, tuple._2, tuple._3)
  implicit def asBoolean(tuple: (Int, Int, Int, Int)): (Boolean, Boolean, Boolean, Boolean) =
    	(tuple._1, tuple._2, tuple._3, tuple._4)
  implicit def asBoolean(tuple: ((Int, Int, Int), (Int, Int, Int, Int))): ((Boolean, Boolean, Boolean), (Boolean, Boolean, Boolean, Boolean)) =
    	(tuple._1, tuple._2)

  test("complex demux") {
    val in, c1, c2, out1, out2, out3, out4 = new Wire
    val outs = List(out1, out2, out3, out4)
    val controls = List(c1, c2)
    demux(in, controls, outs)

    def checkOutputs(exp: Int, value: Boolean, round: Int) {
      run
      for (i <- 1 to outs.size) {
        if (i == exp) {
          assert(outs(i - 1).getSignal === value, s"out$i $round")
        } else {
          assert(outs(i - 1).getSignal === false, s"out$i $round")
        }
      }
    }

    val checks: List[((Boolean, Boolean, Boolean), (Boolean, Boolean, Boolean, Boolean))] = List(
      (0, 0, 0) -> (0, 0, 0, 0),
      (0, 0, 1) -> (0, 0, 0, 0),
      (0, 1, 0) -> (0, 0, 0, 0),
      (0, 1, 1) -> (0, 0, 0, 0),
      (1, 0, 0) -> (1, 0, 0, 0),
      (1, 0, 1) -> (0, 1, 0, 0),
      (1, 1, 0) -> (0, 0, 1, 0),
      (1, 1, 1) -> (0, 0, 0, 1))

    checks.foreach(_ match {
      case (inputs, outputs) => {
        val (_in, _c1, _c2) = inputs
        in.setSignal(_in)
        c1.setSignal(_c1)
        c2.setSignal(_c2)
        run
        val actualOutputs: (Boolean, Boolean, Boolean, Boolean) =
          (out1.getSignal, out2.getSignal, out3.getSignal, out4.getSignal)
        assert(actualOutputs == outputs, s"$inputs: $actualOutputs == $outputs")
      }
    })
  }
}
