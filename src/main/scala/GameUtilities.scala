import Consants.numberToCardMap

import scala.util.Random

case object GameUtilities {
  /*
  Deals a new hand by randomly selecting non-repeating numbers in the range [0, 54)
  and assigning them in a round robin format to each of the players.

  Return - Hand comprising of cards dealt to player1
  */
  def dealNewHand(numberOfPlayers: Int, totalNormalCards: Int): Hand = {

    def dealNextCard(currentPlayer: Int, seenSoFar: List[Int], dealtSoFar: List[Card] ): List[Card] = {
      // Base case
      if(seenSoFar.size == totalNormalCards) return dealtSoFar

      val nextCardNum = Random.nextInt(totalNormalCards)

      // Repeat if encounter same number
      if(seenSoFar.contains(nextCardNum)) dealNextCard(currentPlayer, seenSoFar, dealtSoFar)
      // Else increment player and continue
      else {
        dealNextCard(
          if(currentPlayer == numberOfPlayers) 1
          else currentPlayer + 1,
          seenSoFar :+ nextCardNum,
          if (currentPlayer == 1) dealtSoFar ++ numberToCardMap.get(nextCardNum)
          else dealtSoFar
        )
      }
    }

    Hand(dealNextCard(1, List.empty[Int], List.empty[Card]))

  }
}