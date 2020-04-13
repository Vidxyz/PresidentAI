package ui.panels

import java.awt.geom.Point2D
import java.awt.{Color, Dimension, Graphics2D}

import game.{GameUtilities, Move}
import player.Player
import ui.layouts.BottomLayout
import ui.models.{CardTile, HandCard, PlayerStatusIndicator}

import scala.swing.event.MousePressed
import scala.swing.{Panel, SimpleSwingApplication}

object PlayerHandPanel {
  val width = 600
  val height = 300
  val maxHandSpreadAngle = 180d
  val maxPossibleCardsInHand = 27
  val backgroundColor = new Color(255, 219, 172)
}

class PlayerHandPanel(app: SimpleSwingApplication, var player: Player, parent: BottomLayout) extends Panel {

  import PlayerHandPanel._

  background = backgroundColor
  preferredSize = new Dimension(width, height)
  minimumSize = new Dimension(width, height)
//  border = Swing.LineBorder(Color.BLACK)

  var handToDisplay = GameUtilities.sortCards(player.hand.listOfCards)
  var numberOfCardsInHand = handToDisplay.size
  var maxAngle = maxHandSpreadAngle * numberOfCardsInHand/maxPossibleCardsInHand
  var angleList = -numberOfCardsInHand/2 to numberOfCardsInHand/2
  var handCardList = (handToDisplay zip angleList).zipWithIndex.map( {
    case ((card, angleIndex), cardIndex) =>
      HandCard(card, app, this, cardIndex, numberOfCardsInHand, angleIndex * (maxAngle / numberOfCardsInHand))
  })
  var playerStatusIndicator = PlayerStatusIndicator(app, true)

  focusable = true
  listenTo(mouse.clicks)

  reactions += {
    case e: MousePressed => {
      handCardList = handCardList.map(card => if(card.pointInside(e.point)) card.copy(isSelected = !card.isSelected) else card)
      val selectedCards = handCardList.filter(card => card.pointInside(e.point))
      if(selectedCards.nonEmpty) {
        selectedCards.foreach(p => println(p.card))
        parent.updateCardSelectStatusViaHandCard(handCardList)
      }
      revalidate()
      repaint()
    }
  }

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    handCardList.foreach(h => h.drawSprite(g))
    playerStatusIndicator.drawSprite(g)
  }

  def highestCardInGivenPoint(p: Point2D): Int = {
    handCardList.filter(c => c.rectangle.checkIfContains(p)).map(c => c.rectangle.zVal).max
  }

  def updateCardSelectStatus(cardTileListUi: List[CardTile]) = {
    handCardList = (handCardList zip cardTileListUi).map({case (handCard, cardTile) => handCard.copy(isSelected = cardTile.isSelected)})
    revalidate()
    repaint()
  }

  def setCardsAsSelected(move: Option[Move]) = {
    handCardList = handCardList.map(e => if(move.get.cards.contains(e.card)) e.copy(isSelected = true) else e.copy(isSelected = false))
    revalidate()
    repaint()
  }

  def updateRealPlayerObject(realPlayer: Player) = {
    this.player = realPlayer
    handToDisplay = GameUtilities.sortCards(player.hand.listOfCards)
    numberOfCardsInHand = handToDisplay.size
    maxAngle = maxHandSpreadAngle * numberOfCardsInHand/maxPossibleCardsInHand
    angleList = -numberOfCardsInHand/2 to numberOfCardsInHand/2
    handCardList = (handToDisplay zip angleList).zipWithIndex.map( {
      case ((card, angleIndex), cardIndex) =>
        HandCard(card, app, this, cardIndex, numberOfCardsInHand, angleIndex * (maxAngle / numberOfCardsInHand))
    })
    revalidate()
    repaint()
  }

  def resetSelectionOnCards: Unit = {
    handCardList = handCardList.map(handCard => handCard.copy(isSelected = false))
    revalidate()
    repaint()
  }

  def setPlayerAvatarStatus(currentTurn: Boolean): Unit = {
    playerStatusIndicator = PlayerStatusIndicator(app, currentTurn)
    revalidate()
    repaint()
  }

}
