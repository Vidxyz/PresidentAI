import game.FaceValue._
import game.Suits._
import game.{Game, GameUtilities, Hand, Joker, Move, NormalCard, Round, SpecialCard, WildCard}

import ui.MainLayout

import scala.swing.{Frame, MainFrame, Panel, SimpleSwingApplication}

object Main extends SimpleSwingApplication {

  var currentState = Move(List(NormalCard(SIX, Diamond), NormalCard(SIX, Club)))
//  val listOfNames = List("Player1", "Player2" )
  val listOfNames = List("Player1", "Player2", "Player3", "Player4")
//  val listOfNames = List("Player1", "Player2", "Player3", "Player4", "p5", "p6")
//  val listOfNames = List("Player1", "Player2", "Player3", "Player4", "Player5", "Player6", "Player7", "Player8")
//  val listOfNames = List("Player1", "Player2", "Player3", "Player4", "p5", "p6", "p7", "p8", "PPlayer1", "PPlayer2", "PPlayer3", "PPlayer4", "pP5", "pP6", "p7P", "p8P")
  val listOfNames2 = List("Player1", "Player2")
  // Comment out seed for true randomness
  //  val listOfPlayers = game.GameUtilities.generatePlayersAndDealHands(listOfNames, seed=5).toBuffer
  // 77 and 13 are good seeds
  //  val listOfPlayers = GameUtilities.generatePlayersAndDealHands(listOfNames, seed=13).toBuffer
  //  val listOfPlayers = GameUtilities.generatePlayersAndDealHands(listOfNames).toBuffer
  val listOfPlayers = GameUtilities.generatePlayersAndDealHands(listOfNames)
    .map(player => if(player.name == "Player1") player.copy(isRealPlayer = true) else player).toBuffer
  val listOfPlayers2 = GameUtilities.generatePlayersAndDealHands(listOfNames2, seed=77).toBuffer
  val game = Game(Move(List.empty))

  println("The starting state is : " + game.startState)
  println("\n")
  game.play(listOfPlayers)

  lazy val mainLayout = new MainLayout(this, listOfPlayers.toList)

  def top: Frame = new MainFrame {
    title = "President Card Game"
    contents = mainLayout
    resizable = false
  }



}




