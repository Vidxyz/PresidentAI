package ui.panels

import java.awt.{Color, Dimension}

import game.Card
import ui.models.{ComputerHandCard, ComputerPlayer}

import scala.swing.{Font, Graphics2D, Panel, SimpleSwingApplication, Swing}

object ComputerPlayerAvatarPanel {
  val width = 200
  val height = 200
  val maxHandSpreadAngle = 180d
  val maxPossibleCardsInHand = 27
  val backgroundColor = new Color(4,6,84)
  val fontSize = 18
  val fontName = "TimesRoman"
}

class ComputerPlayerAvatarPanel(app: SimpleSwingApplication,
                                val playerName: String,
                                var playerHand: List[Card],
                                var hasPlayerCompleted: Boolean = false,
                                var isPlayerBum: Boolean = false) extends Panel {
  import ComputerPlayerAvatarPanel._

  background = backgroundColor
  preferredSize = new Dimension(width, height)
  border = Swing.LineBorder(Color.BLACK)

  val computerPlayer = ComputerPlayer(app, playerName)

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
      computerHandList.foreach(_.drawSprite(g))
      computerPlayer.drawSprite(g)
      if(isPlayerBum) {
        computerPlayer.drawSprite(g)
        g.setColor(Color.red)
        g.setFont(Font(fontName, Font.Bold, fontSize))
        g.drawString("BUM", 5 , height - 50)
      }
    }
    // This could mean that player doesn't exist, or has completed
    else {
      if(hasPlayerCompleted) {
        computerPlayer.drawSprite(g)
        g.setColor(Color.red)
        g.setFont(Font(fontName, Font.Bold, fontSize))
        g.drawString("DONE", width/2 - 25 , height - 50)
      }
    }
  }

  def setPlayerAvatarStatus(currentTurn: Boolean): Unit = {
    computerPlayer.updateActivePlayerAvatar(currentTurn)
    revalidate()
    repaint()
  }

  def setPlayerAvatarToComplete: Unit = {
    this.hasPlayerCompleted = true
    revalidate()
    repaint()
  }

  def setPlayerAvatarToBum: Unit = {
    this.isPlayerBum = true
    revalidate()
    repaint()
  }

  def updateUserHasPassedOnRound: Unit = {
    computerPlayer.updateUserHasPassedOnRound
    revalidate()
    repaint()
  }

  def resetUserPassStatus: Unit = {
    if(!hasPlayerCompleted) {
      computerPlayer.resetUserPassStatus
      revalidate()
      repaint()
    }
  }

  def resetUserCompletionStatus: Unit = {
    this.hasPlayerCompleted = false
    this.isPlayerBum = false
    revalidate()
    repaint()
  }

  def updatePlayerHand(newPlayerHand: List[Card]): Unit = {
    this.playerHand = newPlayerHand
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
