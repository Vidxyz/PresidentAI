package neural_network

import neural_network.Transcriber.{datasetInputPath, datasetOutputPath}
import org.deeplearning4j.nn.api.OptimizationAlgorithm
import org.nd4j.linalg.activations.Activation
import org.deeplearning4j.nn.conf.MultiLayerConfiguration
import org.deeplearning4j.nn.conf.NeuralNetConfiguration
import org.nd4j.linalg.learning.config.Nesterovs
import org.deeplearning4j.nn.conf.layers.DenseLayer
import org.deeplearning4j.nn.conf.layers.OutputLayer
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork
import org.deeplearning4j.nn.weights.WeightInit
import org.deeplearning4j.optimize.listeners.{CollectScoresIterationListener, PerformanceListener, ScoreIterationListener}
import org.deeplearning4j.ui.api.UIServer
import org.deeplearning4j.ui.stats.StatsListener
import org.deeplearning4j.ui.storage.InMemoryStatsStorage
import org.nd4j.linalg.factory.Nd4j
import org.nd4j.linalg.lossfunctions.LossFunctions

import scala.io.Source


case class DataSet(dataPoints: List[DataPoint])
case class DataPoint(input: List[Int], output: Long)

class PresidentNeuralNetwork(dataset: DataSet) {

  import PresidentNeuralNetwork._

  val multiLayerConf: MultiLayerConfiguration = new NeuralNetConfiguration.Builder()
    .seed(123)
    .updater(new Nesterovs(0.5, 0.9)) //High Level Configuration
    .optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT)
    .list() //For configuring MultiLayerNetwork we call the list method
    .layer(0, new DenseLayer.Builder()
      .nIn(108)
      .nOut(54)
      .weightInit(WeightInit.XAVIER)
      .activation(Activation.RELU)
      .build()) //Configuring Layers
    .layer(1, new DenseLayer.Builder()
      .nIn(54)
      .nOut(27)
      .weightInit(WeightInit.XAVIER)
      .activation(Activation.RELU)
      .build())
    .layer(2, new DenseLayer.Builder()
      .nIn(27)
      .nOut(14)
      .weightInit(WeightInit.XAVIER)
      .activation(Activation.SIGMOID)
      .build())
    .layer(3, new OutputLayer.Builder(LossFunctions.LossFunction.MEAN_ABSOLUTE_ERROR)
      .nIn(14)
      .nOut(1)
      .weightInit(WeightInit.XAVIER)
      .activation(Activation.IDENTITY)
      .build())
    .build() //Building Configuration

  val model: MultiLayerNetwork = new MultiLayerNetwork(multiLayerConf)


  def init() = {
    //    println(s"Batchsize is ${model.batchSize()}")
    println(s"Config is ${model.conf()}")

    val uiServer = UIServer.getInstance
    val statsStorage = new InMemoryStatsStorage
    uiServer.attach(statsStorage)
    // todo - look at docs for hwo to train and evaluate
    model.init()
    model.setListeners(new ScoreIterationListener(10),
      new PerformanceListener(10),
      new CollectScoresIterationListener(10),
      new StatsListener(statsStorage))
    val inputs = Nd4j.createFromArray(dataset.dataPoints.map(_.input.map(_.toFloat).toArray).toArray)
    val desiredOutputs = Nd4j.createFromArray(dataset.dataPoints.map(p => Array(p.output.toFloat)).toArray)

    for (e <- 1 to epochs) {
      model.fit(inputs, desiredOutputs)
    }

  }
}

object PresidentNeuralNetwork {
  val batchSize = 5
  val epochs = 1000

  def apply(): PresidentNeuralNetwork = {
    val inputSource = Source.fromFile(datasetInputPath)
    val outputSource = Source.fromFile(datasetOutputPath)

    // todo - cross check inputs - figure out results - why is score not increasing?
    val dataPoints: List[DataPoint] =  (for {
      inputLine <- inputSource.getLines()
      outputLine <- outputSource.getLines()
      input = inputLine.split(',').map(_.toInt).toList
      output = outputLine.split(',').map(_.toInt).toList
    } yield DataPoint(input, bitVectorToLong(output))).toList

    val dataSet = DataSet(dataPoints)
    println(dataSet.dataPoints.map(_.output))
    new PresidentNeuralNetwork(dataSet)
  }

  def bitVectorToLong(bitVector: List[Int]): Long =
    bitVector
      .zipWithIndex
      .foldLeft(0L){
        case (acc, (bit, index)) => bit match {
          case 0 => acc
          case 1 => acc + scala.math.pow(2, index).toLong}
      }

  def longToBitVector(long: Long): List[Int] =
    long.toBinaryString.map(_.toInt - 49).toList

}
