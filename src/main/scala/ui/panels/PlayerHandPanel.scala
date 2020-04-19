package ui.panels

import java.awt.geom.Point2D
import java.awt.{Color, Dimension, Graphics2D}

import game.{GameUtilities, Joker, Move}
import player.Player
import ui.layouts.BottomLayout
import ui.models.{CardTile, HandCard, PlayerStatusIndicator}

import scala.swing.event.MousePressed
import scala.swing.{Font, Panel, SimpleSwingApplication}

object PlayerHandPanel {
  val width = 600
  val height = 300
  val maxHandSpreadAngle = 180d
  val maxPossibleCardsInHand = 27
  val backgroundColor = new Color(4,6,84)
  val fontSize = 40
}

class PlayerHandPanel(app: SimpleSwingApplication, var player: Player, parent: BottomLayout,
                      var hasPassedOnRound: Boolean = false,
                      var isPlayerBum: Boolean = false) extends Panel {

  import PlayerHandPanel._

  background = backgroundColor
  preferredSize = new Dimension(width, height)
  minimumSize = new Dimension(width, height)

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
    if(handCardList.isEmpty) {
      g.setColor(Color.red)
      g.setFont(Font("TimesRoman", Font.Bold, fontSize))
      g.drawString("OVER", width/2, height/2)
    }
    else handCardList.foreach(h => h.drawSprite(g))

    playerStatusIndicator.drawSprite(g)

    if(hasPassedOnRound) {
      g.setColor(Color.red)
      g.setFont(Font("TimesRoman", Font.Bold, fontSize*2/3))
      g.drawString("PASS", 25, fontSize)
    }

    // When player is bum, hand is never empty
    if(isPlayerBum) {
      g.setColor(Color.red)
      g.setFont(Font("TimesRoman", Font.Bold, fontSize*2/3))
      g.drawString("BUM", 25, fontSize)
    }
  }

  def highestCardInGivenPoint(p: Point2D): Int = {
    handCardList.filter(c => c.rectangle.checkIfContains(p)).map(c => c.rectangle.zVal).max
  }

  def updateCardSelectStatus(cardTileListUi: List[CardTile]) = {
    handCardList = (handCardList zip cardTileListUi).map({case (handCard, cardTile) => handCard.copy(isSelected = cardTile.isSelected)})
    revalidate()
    repaint()
  }

  def setCardsAsSelected(move: Move) = {
    handCardList = move match {
      case Move(List(Joker), _) => {
        var hasJokerBeenFound = false
        handCardList.map(e =>
          if(move.cards.contains(e.card) && !hasJokerBeenFound) {
            hasJokerBeenFound = true
            e.copy(isSelected = true)
          }
          else e.copy(isSelected = false))
      }
      case move => handCardList.map(e => if(move.cards.contains(e.card)) e.copy(isSelected = true) else e.copy(isSelected = false))
    }
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

  def updateUserHasPassedOnRound: Unit = {
    this.hasPassedOnRound = true
    revalidate()
    repaint()
  }

  def resetUserPassStatus: Unit = {
    this.hasPassedOnRound = false
    revalidate()
    repaint()
  }

  def setPlayerAvatarToBum: Unit = {
    this.isPlayerBum = true
    revalidate()
    repaint()
  }

  def resetPlayerCompletionStatus: Unit = {
    this.isPlayerBum = false
    revalidate()
    repaint()
  }

}
