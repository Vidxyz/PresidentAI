package neural_network

import java.io.{BufferedWriter, File, FileWriter}

import game.{Card, DummyCard, Game, GameData, Hand, Move}
import utils.Constants
import utils.Constants.numberToCardMap

trait Observer[S, D] {
  def receiveUpdate(subject: S, data: D)
}

trait Subject[S, D] {
  this: S =>
  private var observers: List[Observer[S, D]] = Nil
  def addObserver(observer: Observer[S, D]) = observers = observer :: observers
  def notifyObservers(data: D) = observers.foreach(_.receiveUpdate(this, data))
}

trait DatasetCreator {
  def generateInputValue(hand: Hand, gameState: Move): List[Int]
  def generateOutputValue(movePlayed: Move): List[Int]
  def appendToDataSet(input: List[Int], output: List[Int])
}

// todo - there is a bug here with user passing when new game is created - leading to incorrect dataset being created - must fix
class Transcriber extends DatasetCreator with Observer[Game, GameData] {
  import Transcriber._

  /**
   * Generates a binary list of size 54 * 2 = 128
   * Set bits of the first 54 indices represent the cards present in player's hand
   * Set bits of the last 54 indices represent the cards present in current gameState
   * @param hand - The player's hand
   * @param gameState - The current gameState
   * @return - Binary vector comprising of cards in hand, cards in gameState
   */
  override def generateInputValue(hand: Hand, gameState: Move): List[Int] = {
    val handCardValues = hand.listOfCards.map(getCardIntegerValue)
    val gameStateCardValues = gameState.cards.map(getCardIntegerValue)
    ((0 until Constants.totalNumberOfCards).map(x => if(handCardValues.contains(x)) 1 else 0) ++
      (0 until Constants.totalNumberOfCards).map(x => if(gameStateCardValues.contains(x)) 1 else 0)).toList
  }

  /**
   * Generates a binary list of size 54
   * Set bits represent the cards played in move
   * @param movePlayed - Move played by real player
   * @return - Binary vector comprising of cards in move played
   */
  override def generateOutputValue(movePlayed: Move): List[Int] = {
    val moveCardValues = movePlayed.cards.map(getCardIntegerValue)
    (0 until Constants.totalNumberOfCards).map(x => if(moveCardValues.contains(x)) 1 else 0).toList
  }

  /**
   * Appends to dataset files, currently defined as `data/inputs.txt` and `data/outputs.txt`
   * @param input - Binary list of size 128
   * @param output - Binary list of size 54
   * Throws exception if input/output sizes don't match
   */
  override def appendToDataSet(input: List[Int], output: List[Int]): Unit = {
    if(input.size != Constants.totalNumberOfCards * 2) throw InvalidInputSizeException(s"Expected input size ${Constants.totalNumberOfCards * 2}, got ${input.size}")
    if(output.size != Constants.totalNumberOfCards) throw InvalidOutputSizeException(s"Expected output size ${Constants.totalNumberOfCards}, got ${output.size}")

    val inputWriter = new BufferedWriter(new FileWriter(new File(datasetInputPath), true))
    val outputWriter = new BufferedWriter(new FileWriter(new File(datasetOutputPath), true))
    inputWriter.write(input.mkString(",") + '\n')
    outputWriter.write(output.mkString(",") + '\n')
    inputWriter.flush()
    outputWriter.flush()
    inputWriter.close()
    outputWriter.close()
    println("Transcriber: Appended to dataset")
  }

  override def receiveUpdate(subject: Game, gameData: GameData): Unit = {
    val translatedInputs = generateInputValue(gameData.currentHand, gameData.gameState)
    val translatedOutputs = generateOutputValue(gameData.movePlayed.getOrElse(Move(List.empty)))
    appendToDataSet(translatedInputs, translatedOutputs)
  }
}

case object Transcriber {
  val datasetInputPath = "data/real_player_inputs.txt"
  val datasetOutputPath = "data/real_player_outputs.txt"

  def getCardIntegerValue(card: Card): Int = numberToCardMap.find(_._2 == card).map(_._1).getOrElse(-1)

  /**
   * Transform input vector of size 128 into a player hand, and a gameState
   * @param input - input vector from `data/real_player_inputs.txt`
   * @return - a tuple consisting of the deconstructed Hand object, and the gameState
   */
  def deconstructInput(input: List[Int]): (Hand, Move) = {
    if(input.size != Constants.totalNumberOfCards * 2) throw InvalidInputSizeException(s"Expected input size ${Constants.totalNumberOfCards * 2}, got ${input.size}")

    val handList = input.slice(0, Constants.totalNumberOfCards)
    val gameStateList = input.slice(Constants.totalNumberOfCards, Constants.totalNumberOfCards * 2)
    (
      Hand(handList.zipWithIndex.map{case (value, index) => if (value == 1) Constants.numberToCardMap.getOrElse(index, DummyCard) else DummyCard}.filter(_ != DummyCard)),
      Move(gameStateList.zipWithIndex.map{case (value, index) => if (value == 1) Constants.numberToCardMap.getOrElse(index, DummyCard) else DummyCard}.filter(_ != DummyCard))
    )

  }

  /**
   * Transform output vector of size 54 into a move
   * @param output - output vector from `data/real_player_outputs.txt`
   * @return - a move representing the move played by the player, for given input
   */
  def deconstructOutput(output: List[Int]): Move = {
    if(output.size != Constants.totalNumberOfCards) throw InvalidOutputSizeException(s"Expected output size ${Constants.totalNumberOfCards}, got ${output.size}")

    val moveList = output.slice(0, Constants.totalNumberOfCards)
    Move(moveList.zipWithIndex.map{case (value, index) => if (value == 1) Constants.numberToCardMap.getOrElse(index, DummyCard) else DummyCard}.filter(_ != DummyCard))
  }
}

case class InvalidInputSizeException(s: String) extends IllegalArgumentException(s)
case class InvalidOutputSizeException(s: String) extends IllegalArgumentException(s)