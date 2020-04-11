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

class PlayerCardTilePanel(app: SimpleSwingApplication, player: Player, parent: BottomLayout) extends GridBagPanel  {
  import PlayerCardTilePanel._

  val handToDisplay = GameUtilities.sortCards(player.hand.listOfCards)
  background = backgroundColor
//  border = Swing.LineBorder(Color.BLACK)

  preferredSize = new Dimension(width, height)
  minimumSize = new Dimension(width, height)

  val c: Constraints = new Constraints()
  val totalCards = player.hand.size
  var cardTileUiList = handToDisplay.zipWithIndex.map({ case (c,i) => CardTile(app, c, i, totalCards)})

  focusable = true
  listenTo(mouse.clicks, mouse.moves, keys)

  reactions += {
    case e: MousePressed => {
      cardTileUiList.filter(card => card.pointInside(e.point)).foreach(c => println(c.card))
      cardTileUiList = cardTileUiList.map(card => if(card.pointInside(e.point)) card.copy(isSelected = !card.isSelected) else card)
      parent.updateCardSelectStatusViaCardTile(cardTileUiList)
      repaint()
    }
  }

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    cardTileUiList.foreach(c => c.drawSprite(g))
  }

  def updateCardSelectStatus(handCardUiList: List[HandCard]) = {
    cardTileUiList = (cardTileUiList zip handCardUiList).map({case (cardTile, handCard) => cardTile.copy(isSelected = handCard.isSelected)})
    repaint()
  }

  def setCardsAsSelected(move: Option[Move]) = {
    cardTileUiList = cardTileUiList.map(e => if(move.get.cards.contains(e.card)) e.copy(isSelected = true) else e.copy(isSelected = false))
    repaint()
  }

}
