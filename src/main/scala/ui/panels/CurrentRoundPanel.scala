package ui.panels

import java.awt.Color
import java.awt.geom.AffineTransform

import game.Round
import javax.swing.ImageIcon
import ui.models.RoundCard

import scala.swing.{Dimension, Font, Graphics2D, Panel, SimpleSwingApplication, Swing}

object CurrentRoundPanel {
  val width = 1000
  val height = 400
  val backgroundColor = new Color(0,102, 0)
  val fontSize = 20
  val fontName = "TimesRoman"
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

  val backgroundImage = new ImageIcon(app.resourceFromClassloader("assets/round/background_table.png")).getImage
  val affineTransform = new AffineTransform()
  val xScale = width*1.0d/backgroundImage.getWidth(null)
  val yScale = height*1.0d/backgroundImage.getHeight(null)
  affineTransform.scale(xScale, yScale)

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    g.drawImage(backgroundImage, affineTransform, null)
    roundCardUiList.foreach(roundCard => roundCard.drawSprite(g))
    g.setColor(Color.white)
    g.setFont(Font(fontName, Font.Plain, fontSize))
    g.drawString(s"# Moves Played : ${round.movesPlayed.size}", 5, 50)
  }

  def updateRoundObject(newRound: Round) = {
    this.round = newRound
    handToDisplay = if(round == null) List.empty else round.movesPlayed.flatMap(move => move.cards)
    roundCardUiList = handToDisplay.zipWithIndex.map({
      case (card, index) => RoundCard(card, app, index)
    })
    revalidate()
    repaint()
  }
}
