import GameUtilities._

case class Player(name: String, hand: Hand) {

  lazy val status: PlayerStatus = if (hand.listOfCards.isEmpty) Complete else Active

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
