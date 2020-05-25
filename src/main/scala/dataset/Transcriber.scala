package dataset

import java.io.{BufferedWriter, File, FileWriter}

import game.{GameUtilities, Hand, Move}
import org.nd4j.linalg.lossfunctions.ILossFunction
import utils.Constants

trait DatasetCreator {
  def generateInputValue(hand: Hand, gameState: Move): List[Int]
  def generateOutputValue(movePlayed: Move): List[Int]
  def appendToDataSet(input: List[Int], output: List[Int])
}

class Transcriber extends DatasetCreator {
  import Transcriber._

  /**
   * Generates a binary list of size 54 * 2 = 128
   * Set bits of the first 54 indices represent the cards present in player's hand
   * Set bits of the last 54 indices represent the cards present in current gameState
   * @param hand - The player's hand
   * @param gameState - The current gameState
   * @return
   */
  override def generateInputValue(hand: Hand, gameState: Move): List[Int] = {
    val handCardValues = hand.listOfCards.map(GameUtilities.cardOrderValue)
    val gameStateCardValues = gameState.cards.map(GameUtilities.cardOrderValue)
    ((0 until Constants.totalNumberOfCards).map(x => if(handCardValues.contains(x)) 1 else 0) ++
      (0 until Constants.totalNumberOfCards).map(x => if(gameStateCardValues.contains(x)) 1 else 0)).toList
  }

  /**
   * Generates a binary list of size 54
   * Set bits represent the cards played in move
   * @param movePlayed - Move played by real player
   * @return
   */
  override def generateOutputValue(movePlayed: Move): List[Int] = {
    val moveCardValues = movePlayed.cards.map(GameUtilities.cardOrderValue)
    (0 until Constants.totalNumberOfCards).map(x => if(moveCardValues.contains(x)) 1 else 0).toList
  }

  /**
   * Appends to dataset files, currently defined as `data/inputs.txt` and `data/outputs.txt`
   * @param input - Binary list of size 128
   * @param output - Binary list of size54
   * Throws exception if input/output sizes don't match
   */
  override def appendToDataSet(input: List[Int], output: List[Int]): Unit = {
    if(input.size != Constants.totalNumberOfCards * 2) throw InvalidInputSizeException(s"Expected input size ${Constants.totalNumberOfCards * 2}, got ${input.size}")
    if(output.size != Constants.totalNumberOfCards) throw InvalidOutputSizeException(s"Expected output size ${Constants.totalNumberOfCards}, got ${output.size}")

    val inputWriter = new BufferedWriter(new FileWriter(new File(datasetInputPath), true))
    val outputWriter = new BufferedWriter(new FileWriter(new File(datasetOutputPath), true))
    inputWriter.write(input.toString)
    outputWriter.write(output.toString)
  }

}

case object Transcriber {
  val datasetInputPath = "data/inputs.txt"
  val datasetOutputPath = "data/outputs.txt"
}

case class InvalidInputSizeException(s: String) extends IllegalArgumentException(s)
case class InvalidOutputSizeException(s: String) extends IllegalArgumentException(s)