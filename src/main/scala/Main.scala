object Main extends App {

  val numberOfPlayers = 4
  val totalNormalCards = 54

  val sampleHand: Hand = GameUtilities.dealNewHand(numberOfPlayers, totalNormalCards)

  val AI = Player("AI", sampleHand)

  print(AI.hand)

  println('\n')

  val newHand = AI.hand.sortCards

  print(newHand)

  println("\n")
  val sets = newHand.getListOfIntermediateSets

  print(sets)

}




