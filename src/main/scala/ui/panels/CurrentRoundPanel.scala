package ui.panels

import java.awt.Color

import game.{GameUtilities, Round}
import ui.models.RoundCard

import scala.swing.{Dimension, Graphics2D, Panel, SimpleSwingApplication, Swing}

object CurrentRoundPanel {
  val width = 800
  val height = 300
  val backgroundColor = new Color(0,102, 0)
}

class CurrentRoundPanel(app: SimpleSwingApplication, var round: Round) extends Panel {
  import CurrentRoundPanel._

  background = backgroundColor
  preferredSize = new Dimension(width, height)
  minimumSize = new Dimension(width, height)
  border = Swing.LineBorder(Color.BLACK)

  var handToDisplay = if(round == null) List.empty else round.movesPlayed.flatMap(move => move.cards)
  var roundCardUiList = handToDisplay.zipWithIndex.map({
    case (card, index) => RoundCard(card, app, index)
  })

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    roundCardUiList.foreach(roundCard => roundCard.drawSprite(g))
    g.setColor(Color.white)
    g.drawString(s"# Moves Played : ${round.movesPlayed.size}", 5, 50)
  }

  def updateRoundObject(newRound: Round) = {
    this.round = newRound
    handToDisplay = round.movesPlayed.flatMap(move => move.cards)
    roundCardUiList = handToDisplay.zipWithIndex.map({
      case (card, index) => RoundCard(card, app, index)
    })
    revalidate()
    repaint()
  }
}
