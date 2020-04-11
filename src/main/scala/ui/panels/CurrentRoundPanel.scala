package ui.panels

import java.awt.Color
import java.awt.geom.AffineTransform

import game.Round
import ui.models.RoundCard
import utils.Consants._

import scala.swing.{Dimension, Graphics2D, Panel, SimpleSwingApplication, Swing}

object CurrentRoundPanel {
  val width = 800
  val height = 300
  val backgroundColor = new Color(0,102, 0)
}

class CurrentRoundPanel(app: SimpleSwingApplication, round: Round) extends Panel {
  import CurrentRoundPanel._

  background = backgroundColor
  preferredSize = new Dimension(width, height)
  minimumSize = new Dimension(width, height)
  border = Swing.LineBorder(Color.BLACK)

  //  val handToDisplay = GameUtilities.sortCards(player.hand.listOfCards)
  val handToDisplay = List(SEVEN_Club, SEVEN_Heart, NINE_Heart, NINE_Spade, TEN_Diamond, TEN_Club, QUEEN_Spade, QUEEN_Heart, KING_Spade,KING_Heart)

  var affineTransform = new AffineTransform()
  var roundCardUiList = handToDisplay.zipWithIndex.map({
    case (card, index) => RoundCard(card, app, index)
  })

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    roundCardUiList.foreach(roundCard => roundCard.drawSprite(g))
  }

}
