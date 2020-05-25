package dataset

import java.lang

import org.nd4j.linalg.activations.IActivation
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.lossfunctions.ILossFunction
import org.nd4j.linalg.primitives.Pair

class PresidentLossFunction extends ILossFunction {

  override def computeScore(labels: INDArray, preOutput: INDArray, activationFn: IActivation, mask: INDArray, average: Boolean): Double = ???

  override def computeScoreArray(labels: INDArray, preOutput: INDArray, activationFn: IActivation, mask: INDArray): INDArray = ???

  override def computeGradient(labels: INDArray, preOutput: INDArray, activationFn: IActivation, mask: INDArray): INDArray = ???

  override def computeGradientAndScore(labels: INDArray, preOutput: INDArray, activationFn: IActivation, mask: INDArray, average: Boolean): Pair[lang.Double, INDArray] = ???

  override def name(): String = "PresidentLossFunction"
}
