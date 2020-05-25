package neural_network

import java.lang

import org.nd4j.linalg.activations.IActivation
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.lossfunctions.ILossFunction
import org.nd4j.linalg.ops.transforms.Transforms
import org.nd4j.linalg.primitives.Pair

class PresidentLossFunction extends ILossFunction {

  // todo - re-implement - this is example
  private def getScoreArray(labels: INDArray, preOutput: INDArray, activationFn: IActivation, mask: INDArray): INDArray = {
    val output = activationFn.getActivation(preOutput.dup, true)
    val yMinusyHat = Transforms.abs(labels.sub(output))
    if(mask != null) yMinusyHat.mul(yMinusyHat).addi(yMinusyHat).muliColumnVector(mask)
    else yMinusyHat.mul(yMinusyHat).addi(yMinusyHat)
  }

  // todo - implement
  override def computeGradient(labels: INDArray, preOutput: INDArray, activationFn: IActivation, mask: INDArray): INDArray = {
    val output = activationFn.getActivation(preOutput.dup, true)
    val yMinusyHat = labels.sub(output)
    val dl_dyhat = yMinusyHat.mul(-2).sub(Transforms.sign(yMinusyHat)) // this line might have to change
    // This should remain the same... apa
    if(mask != null) activationFn.backprop(preOutput.dup, dl_dyhat).getFirst.muliColumnVector(mask)
    else activationFn.backprop(preOutput.dup, dl_dyhat).getFirst
  }

  /***
   * The following methods are boilerplate
   * For more info, refer to `https://github.com/eclipse/deeplearning4j-examples/blob/master/dl4j-examples/src/main/java/org/deeplearning4j/examples/misc/lossfunctions/CustomLossL1L2.java`
   */
  override def computeScore(labels: INDArray, preOutput: INDArray, activationFn: IActivation, mask: INDArray, average: Boolean): Double = {
    val scoreArray = getScoreArray(labels, preOutput, activationFn, mask)
    if(average) scoreArray.sumNumber().doubleValue() /= scoreArray.size(0)
    else scoreArray.sumNumber().doubleValue()
  }

  override def computeScoreArray(labels: INDArray, preOutput: INDArray, activationFn: IActivation, mask: INDArray): INDArray =
    getScoreArray(labels, preOutput, activationFn, mask).sum(1)

  override def computeGradientAndScore(labels: INDArray, preOutput: INDArray, activationFn: IActivation, mask: INDArray, average: Boolean): Pair[lang.Double, INDArray] =
    new Pair(computeScore(labels, preOutput, activationFn, mask), computeGradient(labels, preOutput, activationFn, mask))

  override def name(): String = "PresidentLossFunction"
}
