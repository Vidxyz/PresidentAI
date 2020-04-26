package ui.panels

import java.awt.Color

import game.{Card, GameUtilities}
import javax.swing.SpringLayout.Constraints
import ui.layouts.UserPromptDialogLayout
import ui.models.CardTile

import scala.swing.event.MousePressed
import scala.swing.{Dimension, Graphics2D, Panel, SimpleSwingApplication, Swing}
import scala.collection.immutable.Queue


object UserPromptCardTilePanel {
  val width = 600
  val height = 300
  val backgroundColor = new Color(4,6,84)
  val maxRows = 4
}

class UserPromptCardTilePanel(app: SimpleSwingApplication, cards: List[Card], cardsToDrop: Int) extends Panel {
  import UserPromptCardTilePanel._

  background = backgroundColor
  preferredSize = new Dimension(width, height)
  minimumSize = new Dimension(width, height)
  val c: Constraints = new Constraints()

  class FiniteQueue[A](q: Queue[A]) {
    def enqueueFinite[B >: A](elem: B, maxSize: Int): Queue[B] = {
      var ret = q.enqueue(elem)
      while (ret.size > maxSize) { ret = ret.dequeue._2 }
      ret
    }
  }
  implicit def queue2finitequeue[A](q: Queue[A]) = new FiniteQueue[A](q)

  var totalCards = cards.size
  var handToDisplay = GameUtilities.sortCards(cards)
  var cardTileList = handToDisplay.zipWithIndex.map({ case (c,i) => CardTile(app, c, maxRows, i, totalCards)})
  var selectedCardsQueue = Queue[Card]()

  focusable = true
  listenTo(mouse.clicks)

  // todo - need to reexchange hands when re-deal happens in round
  // todo - need to handle edge case situation of joker selection here
  reactions += {
    case e: MousePressed => {
      cardTileList.foreach(card => if(card.pointInside(e.point)) selectedCardsQueue = selectedCardsQueue.enqueueFinite(card.card, cardsToDrop))
      cardTileList = cardTileList.map(card => if(selectedCardsQueue.contains(card.card)) card.copy(isSelected = true) else card.copy(isSelected = false))
      revalidate()
      repaint()
    }
  }

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    cardTileList.foreach(_.drawSprite(g))
  }

  private def getSelectedCardsSize = cardTileList.count(c => c.isSelected)

}
