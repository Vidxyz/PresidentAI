package ui.panels

import java.awt.{Color, Dimension}

import game.Card
import player.Player
import ui.models.{ComputerHandCard, ComputerPlayer}
import utils.Consants._

import scala.swing.{Graphics2D, Panel, SimpleSwingApplication, Swing}

object ComputerPlayerAvatarPanel {
  val width = 300
  val height = 300
  val maxHandSpreadAngle = 180d
  val maxPossibleCardsInHand = 27
}

class ComputerPlayerAvatarPanel(app: SimpleSwingApplication, var playerHand: List[Card], var hasPlayerCompleted: Boolean = false) extends Panel {
  import ComputerPlayerAvatarPanel._

  background = Color.white
  preferredSize = new Dimension(width, height)
  border = Swing.LineBorder(Color.BLACK)

  val computerPlayer = ComputerPlayer(app)

  var numberOfCardsInHand = playerHand.size
  var maxAngle = maxHandSpreadAngle * numberOfCardsInHand/maxPossibleCardsInHand
  var angleList = -numberOfCardsInHand/2 to numberOfCardsInHand/2
  var computerHandList = (playerHand zip angleList).zipWithIndex.map( {
    case ((card, angleIndex), cardIndex) =>
      ComputerHandCard(card, app, cardIndex, numberOfCardsInHand, angleIndex * (maxAngle / numberOfCardsInHand))
  })

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    if(playerHand.nonEmpty) {
      computerPlayer.drawSprite(g)
      computerHandList.foreach(_.drawSprite(g))
    }
  }

  def setPlayerAvatarStatus(currentTurn: Boolean): Unit = {
    if(!hasPlayerCompleted) {
      computerPlayer.updateActivePlayerAvatar(currentTurn)
      revalidate()
      repaint()
    }
  }

  def setPlayerAvatarToComplete(): Unit = {
    this.hasPlayerCompleted = true
    computerPlayer.setPlayerAvatarToComplete()
    revalidate()
    repaint()
  }

  def displayUserHasPassedOnRound(): Unit = {
    computerPlayer.displayUserHasPassedOnRound()
    revalidate()
    repaint()
  }

  def updatePlayerObject(player: Player): Unit = {
    this.playerHand = player.hand.listOfCards
    numberOfCardsInHand = playerHand.size
    maxAngle = maxHandSpreadAngle * numberOfCardsInHand/maxPossibleCardsInHand
    angleList = -numberOfCardsInHand/2 to numberOfCardsInHand/2
    computerHandList = (playerHand zip angleList).zipWithIndex.map( {
      case ((card, angleIndex), cardIndex) =>
        ComputerHandCard(card, app, cardIndex, numberOfCardsInHand, angleIndex * (maxAngle / numberOfCardsInHand))
    })
    revalidate()
    repaint()
  }
}
