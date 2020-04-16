import game.{Game, GameUtilities, Hand, Move, NormalCard}
import ui.MainLayout

import scala.swing.{Frame, MainFrame, SimpleSwingApplication, Swing}
import utils.Consants._

object Main extends SimpleSwingApplication {

//  val listOfNames = List("Player1" )
//  val listOfNames = List("Player1", "Player2" )
//  val listOfNames = List("Player1", "Player2", "Player3")
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

  lazy val mainLayout = new MainLayout(this, listOfPlayers.toList)

  val game = Game(Move(List.empty), listOfPlayers, mainLayout)
  println("The starting state is : " + game.startState)
  println("\n")

  val thread = new Thread {
    override def run {
      game.play()
    }
  }
  thread.start()


  def top: Frame = new MainFrame {
    title = "President Card Game"
    contents = mainLayout
    resizable = true
  }

}




