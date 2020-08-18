package neural_network

import neural_network.Transcriber.{datasetInputPath, datasetOutputPath}
import org.datavec.api.records.reader.RecordReader
import org.datavec.api.records.reader.impl.csv.CSVRecordReader
import org.datavec.api.split.FileSplit
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
import org.nd4j.linalg.lossfunctions.LossFunctions
import java.io.{File, PrintWriter}

import org.apache.commons.io.FileUtils
import org.deeplearning4j.datasets.datavec.RecordReaderDataSetIterator
import org.nd4j.evaluation.classification.Evaluation
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.dataset.api.iterator.DataSetIterator
import org.nd4j.linalg.dataset.api.preprocessor.NormalizerMinMaxScaler
import org.nd4j.linalg.factory.Nd4j
import utils.Constants

import scala.io.{BufferedSource, Source}


case class DataSet(dataPoints: List[Observation])
case class Observation(input: List[Int], output: List[Int])

class PresidentNeuralNetwork(inputSource: BufferedSource, outputSource: BufferedSource) {

  import PresidentNeuralNetwork._

  val multilLabelMultiClassConf: MultiLayerConfiguration = new NeuralNetConfiguration.Builder()
    .seed(123)
    .updater(new Nesterovs(0.1, 0.9)) //High Level Configuration
    .optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT)
    .list() //For configuring MultiLayerNetwork we call the list method
    .layer(0, new DenseLayer.Builder()
      .nIn(108)
      .nOut(81)
      .weightInit(WeightInit.XAVIER)
      .activation(Activation.SIGMOID)
      .build()) //Configuring Layers
    .layer(1, new DenseLayer.Builder()
      .nIn(81)
      .nOut(68)
      .weightInit(WeightInit.XAVIER)
      .activation(Activation.RELU)
      .build())
    .layer(2, new OutputLayer.Builder(LossFunctions.LossFunction.MCXENT)
      .nIn(68)
      .nOut(54)
      .biasInit(0.5d)
      .weightInit(WeightInit.XAVIER)
      .activation(Activation.SOFTMAX)
      .build())
    .build() //Building Configuration

  val vectorModel: MultiLayerNetwork = new MultiLayerNetwork(multilLabelMultiClassConf)


  //------ v2
  // 108 -> 54 -> 2 gave 21% accuracyt
//  val continuousOutputConf: MultiLayerConfiguration = new NeuralNetConfiguration.Builder()
//    .seed(123)
//    .updater(new Nesterovs(0.5, 0.9)) //High Level Configuration
//    .optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT)
//    .list() //For configuring MultiLayerNetwork we call the list method
//    .layer(0, new DenseLayer.Builder()
//      .nIn(108)
//      .nOut(81)
//      .weightInit(WeightInit.XAVIER)
//      .activation(Activation.RELU)
//      .build()) //Configuring Layers
//    .layer(1, new DenseLayer.Builder()
//      .nIn(81)
//      .nOut(54)
//      .weightInit(WeightInit.XAVIER)
//      .activation(Activation.RELU)
////      .biasInit(0.2d)
//      .build())
//    .layer(2, new DenseLayer.Builder()
//      .nIn(54)
//      .nOut(27)
//      .weightInit(WeightInit.XAVIER)
//      .activation(Activation.RELU)
////      .biasInit(0.3d)
//      .build())
//    .layer(3, new DenseLayer.Builder()
//      .nIn(27)
//      .nOut(14)
//      .weightInit(WeightInit.XAVIER)
//      .activation(Activation.RELU)
////      .biasInit(0.5d)
//      .build())
//    .layer(4, new OutputLayer.Builder(LossFunctions.LossFunction.XENT)
//      .nIn(14)
//      .nOut(2)
//      .weightInit(WeightInit.XAVIER)
//      .activation(Activation.SIGMOID)
//      .build())
//    .build() //Building Configuration
  // todo - currently at 21.67% accuracy w/ 108->54->2.
  //  10% with 108->2
  //  0% with 108 -> 54 -> 27 -> 2
  val continuousOutputConf: MultiLayerConfiguration = new NeuralNetConfiguration.Builder()
    .seed(123)
    .updater(new Nesterovs(0.5, 0.9)) //High Level Configuration
    .optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT)
    .list() //For configuring MultiLayerNetwork we call the list method
    .layer(0, new DenseLayer.Builder()
      .nIn(108)
      .nOut(54)
//      .dropOut(0.1d)
      .weightInit(WeightInit.XAVIER)
      .activation(Activation.RELU)
//      .biasInit(0.2d)
      .build())
//    .layer(1, new DenseLayer.Builder()
//      .nIn(54)
//      .nOut(27)
//      .weightInit(WeightInit.XAVIER)
//      .activation(Activation.RELU)
//      .build())
    .layer(1, new OutputLayer.Builder(LossFunctions.LossFunction.XENT)
      .nIn(54)
      .nOut(2)
      .weightInit(WeightInit.XAVIER)
      .activation(Activation.SIGMOID)
      .build())
    .build() //Building Configuration

  val continuousModel: MultiLayerNetwork = new MultiLayerNetwork(continuousOutputConf)

  // Ensemble classifier
  def initV3() = {

    val uiServer = UIServer.getInstance
    val statsStorage = new InMemoryStatsStorage
    uiServer.attach(statsStorage)

    val rawInput = inputSource.getLines().toList
    val rawOutput = outputSource.getLines().toList

    val classifiers = (1 to Constants.totalNumberOfCards)
      .map(_ => new MultiLayerNetwork(continuousOutputConf))
      .zipWithIndex
      .map { case (network, index) =>
        // todo - might have to add dual classes here, and then train and test?
        val combinedTrain = (rawInput.take(950) zip rawOutput.take(950))
            .map{case (input, output) => input + ',' + output.split(',')(index) }

        // create combined file out of two
        new PrintWriter(combinedTempFilePath + ".train." + index) {
          write(combinedTrain.mkString("\n"))
          close()
        }

        val rr: RecordReader = new CSVRecordReader()
        rr.initialize(new FileSplit(new File(combinedTempFilePath + ".train." + index)))

        val trainData: DataSetIterator = new RecordReaderDataSetIterator.Builder(rr, batchSize)
          .classification(108, 2)
//          .regression(108, 108)
          .build()

        network.init()
        network.setListeners(
          new CollectScoresIterationListener(10),
          new StatsListener(statsStorage))

        network.fit(trainData, epochs)
        println(s"Finished ${index + 1}/${Constants.totalNumberOfCards}")
        network
      }.toList

      // Test classifiers individually
      val trainedClassifiers = classifiers
        .zipWithIndex
        .map{ case (network, index) =>
          val combinedTest = (rawInput.drop(950) zip rawOutput.drop(950))
            .map{case (input, output) => input + ',' + output.split(',')(index)}

          new PrintWriter(combinedTempFilePath + ".test." + index) {
            write(combinedTest.mkString("\n"))
            close()
          }

          val rrTest: RecordReader = new CSVRecordReader()
          rrTest.initialize(new FileSplit(new File(combinedTempFilePath + ".test." + index)))

          val testData: DataSetIterator = new RecordReaderDataSetIterator.Builder(rrTest, batchSize)
            .classification(108, 2)
//            .regression(108, 108)
            .build()

          // Evaluate model and print results
          val evaluation = network.evaluate[Evaluation](testData)
          println("-----------------------------------")
          println(s"Classifier for ${Constants.numberToCardMap.getOrElse(index, None)} ")
          println("-----------------------------------")
          println("Accuracy (%): " + evaluation.accuracy() * 100)
          println("Precision (%): " + evaluation.precision() * 100)
          println("Recall (%): " + evaluation.recall() * 100)

          FileUtils.deleteQuietly(new File(combinedTempFilePath + ".train." + index))
          FileUtils.deleteQuietly(new File(combinedTempFilePath + ".test." + index))
          network
        }

      // Test classifiers combined to create a move
      // Each classifier is for each individual card
      // Run each classifier through each data point - combine results
      // Each classifier will output a 0-1 result of how likely it is to be played
      // Each test data point will have to run through each classifier, and results, collected
      val testDataInputs = rawInput.drop(950)
        .map(input => input.split(',').map(_.toInt).toList)

      val testDataOutputs = rawOutput.drop(950)
        .map(output => output.split(',').map(_.toInt).toList)

      val dataPoints = testDataInputs.zip(testDataOutputs).map{
        case (input, output) => Observation(input, output)
      }


      val predictions = dataPoints.map { observation =>
        val networkOutputs = trainedClassifiers.map(network => network.output(Nd4j.createFromArray(Array(observation.input.toArray))) )
        // todo - might not have to drop head anymore,
        networkOutputs.map(_.toFloatVector.toList)
      }

      val testInput = rawInput.drop(950).map(input => input.split(',').map(_.toInt).toList )
      val testOutput = rawOutput.drop(950).map(output => output.split(',').map(_.toInt).toList )

      println("Final Predictions combined")
      println("--------------------------")

      predictions.zip(testInput zip testOutput).foreach{ case (prediction, (testInput, expected)) =>
        val mappedPrediction = prediction.map(pred => if(pred.head  > 0.5f) 0 else 1)
        println("Input: " + testInput)
        println("Expected: " + expected)
        println("Actual  : " + mappedPrediction)
        println("Raw  : " + predictions)
        println("Number of expected set  bits : " + expected.count(x => x == 1))
        println("Number of actual   set  bits : " + mappedPrediction.count(x => x == 1))
//        println("TOTAL : " + prediction.foldLeft(0f)((acc, value) => value + acc))
        println("-----------------------------------------")
      }


      val numberOfCorrectPredictions = predictions.zip(testOutput).map {
        case (prediction, expected) =>
          val mappedPrediction = prediction.map(pred => if(pred.head  > 0.5f) 0 else 1)
          mappedPrediction.zip(expected).map {
            case (predictedBit, expectedBit) => if (predictedBit == expectedBit) true else false
          }.forall(acc => acc)
      }.count(x => x)

      val accuracy = (numberOfCorrectPredictions * 1.0f) / (testOutput.size * 1.0f)

      println(s"True classification accuracy = ${accuracy * 100} %")
  }

  // 54 output layer softmax classifier
  def initV1() = {
    println(s"Config is ${vectorModel.conf()}")

    val rawInput = inputSource.getLines().toList
    val rawOutput = outputSource.getLines().toList
    val normalizedOutput = rawOutput.map { line =>
      val binaryList = line.split(',').toList.map(_.toInt)
      val numberOfSetBits = binaryList.count(x => x == 1)
      val normalizedList = binaryList.map(x => if(x == 1) x/numberOfSetBits.toFloat else 0)
      normalizedList.mkString(",")
    }

    val combined = rawInput.zip(normalizedOutput)
      .map{ case (input, output) => input + ',' + output}

    // create combined file out of two
    new PrintWriter(combinedTempFilePath) {
      write(combined.mkString("\n"))
      close()
    }

    val recordReader: RecordReader = new CSVRecordReader()
    recordReader.initialize(new FileSplit(new File(combinedTempFilePath)))

    val trainData: DataSetIterator = new RecordReaderDataSetIterator.Builder(recordReader, batchSize)
      .regression(108, 161)
      .build()


    val uiServer = UIServer.getInstance
    val statsStorage = new InMemoryStatsStorage
    uiServer.attach(statsStorage)

    vectorModel.init()
    vectorModel.setListeners(new ScoreIterationListener(10),
      new PerformanceListener(10),
      new CollectScoresIterationListener(10),
      new StatsListener(statsStorage))

    for (i <- 1 to epochs) {
      println("Epoch " + i + " / " + epochs)
      vectorModel.fit(trainData)
    }

    // Evaluate model and print results
    val evaluation = vectorModel.evaluate[Evaluation](trainData)
    println("Accuracy: " + evaluation.accuracy())
    println("Precision: " + evaluation.precision())
    println("Recall: " + evaluation.recall())

//    FileUtils.deleteQuietly(new File(combinedTempFilePath))



    val testDataInputs = rawInput.drop(950)
      .map(input => input.split(',').map(_.toInt).toList)

    val testDataOutputs = rawOutput.drop(950)
      .map(output => output.split(',').map(_.toInt).toList)

    val dataPoints = testDataInputs.zip(testDataOutputs).map{
      case (input, output) => Observation(input, output)
    }

    val predictions = dataPoints.map { observation =>
      vectorModel.output(Nd4j.createFromArray(Array(observation.input.toArray))).toFloatVector.toList
    }

    val expected = rawOutput.drop(950).map(output => output.split(',').map(_.toInt).toList )

    println("Final Predictions combined")
    println("--------------------------")

    predictions.zip(expected).foreach{ case (prediction, expected) =>
      println("Expected: " + expected)
      println("Actual  : " + prediction.map(x => if(x > 2f/54) 1 else 0))
      println("Actual  : " + prediction)
//      println("TOTAL : " + prediction.foldLeft(0f)((acc, op) => op + acc))
      println("-----------------------------------------")
    }



  }

  def initV2() = {
    //    println(s"Batchsize is ${model.batchSize()}")
    println(s"Config is ${vectorModel.conf()}")

    val combined = (for {
      inputLine <- inputSource.getLines()
      outputLine <- outputSource.getLines()
      reducedOutput = bitVectorToLong(outputLine.split(',').map(_.toInt).toList)
    } yield inputLine + ',' +  reducedOutput).toList


    // create combined file out of two
    new PrintWriter(combinedTempFilePath) {
      write(combined.mkString("\n"))
      close()
    }

    // Create reader and iterator
    val recordReader: RecordReader = new CSVRecordReader()
    recordReader.initialize(new FileSplit(new File(combinedTempFilePath)))

    val trainData: DataSetIterator = new RecordReaderDataSetIterator.Builder(recordReader, batchSize)
      .regression(108, 108)
      .build()

    // Local UI server
    val uiServer = UIServer.getInstance
    val statsStorage = new InMemoryStatsStorage
    uiServer.attach(statsStorage)

    // Train model
    continuousModel.init()
    continuousModel.setListeners(new ScoreIterationListener(10),
      new PerformanceListener(10),
      new CollectScoresIterationListener(10),
      new StatsListener(statsStorage))

    for (i <- 1 to epochs) {
      println("Epoch " + i + " / " + epochs)
      continuousModel.fit(trainData)
    }

    // Evaluate model and print results
    val evaluation = continuousModel.evaluate[Evaluation](trainData)
    println("Accuracy: " + evaluation.accuracy())
    println("Precision: " + evaluation.precision())
    println("Recall: " + evaluation.recall())

    FileUtils.deleteQuietly(new File(combinedTempFilePath))

  }
}

object PresidentNeuralNetwork {
  val batchSize = 15
  val epochs = 100
  val combinedTempFilePath = "data/combined_dataset_temp.csv"

  def apply(): PresidentNeuralNetwork = {
    val inputSource = Source.fromFile(datasetInputPath)
    val outputSource = Source.fromFile(datasetOutputPath)

    // todo - cross check inputs - figure out results - why is score not increasing?
//    val dataPoints: List[DataPoint] =  (for {
//      inputLine <- inputSource.getLines()
//      outputLine <- outputSource.getLines()
//      input = inputLine.split(',').map(_.toInt).toList
//      output = outputLine.split(',').map(_.toInt).toList
//    } yield DataPoint(input, bitVectorToLong(output))).toList
//
//    val dataSet = DataSet(dataPoints)
//    println(dataSet.dataPoints.map(_.output))
    new PresidentNeuralNetwork(inputSource, outputSource)
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
