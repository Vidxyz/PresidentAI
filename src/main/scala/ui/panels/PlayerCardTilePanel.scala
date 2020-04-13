package ui.panels

import java.awt.Color

import game.{GameUtilities, Move}
import player.Player
import ui.layouts.BottomLayout
import ui.models.{CardTile, HandCard}

import scala.swing.event.MousePressed
import scala.swing.{Dimension, Graphics2D, GridBagPanel, SimpleSwingApplication}

object PlayerCardTilePanel {
  val width = 500
  val height = 300
  val backgroundColor = new Color(255, 219, 172)
  val maxRows = 4
}

class PlayerCardTilePanel(app: SimpleSwingApplication, var player: Player, parent: BottomLayout) extends GridBagPanel  {
  import PlayerCardTilePanel._

  background = backgroundColor
  //  border = Swing.LineBorder(Color.BLACK)

  preferredSize = new Dimension(width, height)
  minimumSize = new Dimension(width, height)
  val c: Constraints = new Constraints()

  var totalCards = player.hand.size
  var handToDisplay = GameUtilities.sortCards(player.hand.listOfCards)
  var cardTileList = handToDisplay.zipWithIndex.map({ case (c,i) => CardTile(app, c, i, totalCards)})

  focusable = true
  listenTo(mouse.clicks)

  reactions += {
    case e: MousePressed => {
      cardTileList = cardTileList.map(card => if(card.pointInside(e.point)) card.copy(isSelected = !card.isSelected) else card)
      val selectedCards = cardTileList.filter(card => card.pointInside(e.point))
      if(selectedCards.nonEmpty) {
        selectedCards.foreach(p => println(p.card))
        parent.updateCardSelectStatusViaCardTile(cardTileList)
      }
      revalidate()
      repaint()
    }
  }

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    cardTileList.foreach(c => c.drawSprite(g))
  }

  def updateCardSelectStatus(handCardUiList: List[HandCard]) = {
    cardTileList = (cardTileList zip handCardUiList).map({case (cardTile, handCard) => cardTile.copy(isSelected = handCard.isSelected)})
    revalidate()
    repaint()
  }

  def setCardsAsSelected(move: Option[Move]) = {
    cardTileList = cardTileList.map(e => if(move.get.cards.contains(e.card)) e.copy(isSelected = true) else e.copy(isSelected = false))
    revalidate()
    repaint()
  }

  def updateRealPlayerObject(realPlayer: Player) = {
    this.player = realPlayer
    totalCards = player.hand.size
    handToDisplay = GameUtilities.sortCards(player.hand.listOfCards)
    cardTileList = handToDisplay.zipWithIndex.map({ case (c,i) => CardTile(app, c, i, totalCards)})
    revalidate()
    repaint()
  }

  def resetSelectionOnCards: Unit = {
    cardTileList = cardTileList.map(cardTile => cardTile.copy(isSelected = false))
    revalidate()
    repaint()
  }

}
