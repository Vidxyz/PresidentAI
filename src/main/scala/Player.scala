import GameUtilities._

case class Player(name: String, hand: Hand) {

  lazy val status: PlayerStatus = if (hand.listOfCards.isEmpty) Complete else Active
  implicit lazy val playerIndicators: PlayerIndicators = PlayerIndicators(hand)

  /*
  Returns the move chosen to play, given current hand and current state
   */
  def playNextMove(currentHand: Hand, currentState: Move): Option[Move] = {
    val sortedHand = Hand(sortCards(currentHand.listOfCards))
    val intermediateLists: List[List[Card]] = getListsOfSimilarCards(sortedHand)
    val allMoves: Moves = getAllMoves(intermediateLists)
    val validMoves: Moves = getValidMoves(allMoves, currentState)
    getNextMove(validMoves, currentState)
  }

  /*
  Returns the new hand comprising of cards from currentHand that do not appear in movePlayed
   */
  def getNewHand(currentHand: Hand, movePlayed: Option[Move]): Hand = {
    movePlayed.getOrElse(None) match {
      case move: Move => Hand(
        currentHand
          .listOfCards
          .filter(c => !move.cards.contains(c)))
      case None => currentHand
    }
  }
}


case object PlayerIndicators {

  /*
  Spline interpolation via https://tools.timodenk.com/cubic-spline-interpolation
  Data points used -
  x  |  y
  --------
  1  | 100
  2  | 99
  6  | 50
  10 | 20
  20 | 10
  30 | 0

  Returns a 0-100 Double value of a highCardModifier
   */
  def applyCustomSpecialCardModifier(sizeOfHand: Int): Double = {
    sizeOfHand match {
      case x if 1 to 2 contains x =>
        (-1.364 * Math.pow(x, 3)) + (4.0921 * Math.pow(x, 2)) + (-3.7281 * x) + 101
      case x if 3 to 6 contains x =>
        (0.49041 * Math.pow(x, 3)) + (-7.0346 * Math.pow(x, 2)) + (18.525 * x) + 86.164
      case x if 7 to 10 contains x =>
        (-0.10911 * Math.pow(x, 3)) + (3.7567 * Math.pow(x, 2)) + (-46.222 * x) + 215.66
      case x if 11 to 20 contains x =>
        (-0.020145 * Math.pow(x, 3)) + (1.0878 * Math.pow(x, 2)) + (-19.534 * x) + 126.7
      case x if 21 to 30 contains x =>
        (.004029 * Math.pow(x, 3)) + (-0.36261 * Math.pow(x, 2)) + (9.4755 * x) - 66.697
      case _ => 0
    }
  }
}


case class PlayerIndicators(hand: Hand) {

  import PlayerIndicators._

  // Likelihood of playing a special card. Increases as the game moves on (hand nears empty)
  implicit lazy val specialCardModifier: Double = applyCustomSpecialCardModifier(hand.listOfCards.size)/100
  // Likelihood of passing the next move. TBD
  lazy val probabilityOfPassing: Double = 0.5
}
