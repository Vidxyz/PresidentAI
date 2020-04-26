package ui.layouts

import java.awt.Color

import game.{Card, Hand, PlayerCompletionStatus, President, VicePres}
import ui.panels.{PlayerCompletionUserPromptPanel, UserPromptCardTilePanel}

import scala.swing.event.ButtonClicked
import scala.swing.{Button, Dialog, Dimension, Font, GridBagPanel, Insets, Panel, SimpleSwingApplication, Swing}

object UserPromptDialogLayout {
  val width = 600
  val height = 700
  val fontSize = 24
  val fontName = "TimesRoman"
  val backgroundColor = new Color(4,6,84)
}

class UserPromptDialogLayout(app: SimpleSwingApplication, hand: Hand, parent: Dialog,
                             status: PlayerCompletionStatus, message: String) extends GridBagPanel {
  import UserPromptDialogLayout._

  preferredSize = new Dimension(width, height)
  minimumSize = new Dimension(width, height)
  border = Swing.LineBorder(Color.BLACK)
  background = backgroundColor

  var selectedCards: List[Card] = List.empty
  val c: Constraints = new Constraints()

  c.fill = GridBagPanel.Fill.Vertical
  c.weighty = 1
  c.weightx = 1
  c.gridx = 0
  c.gridy = 0
  layout(new PlayerCompletionUserPromptPanel(status, message)) = c

  c.fill = GridBagPanel.Fill.Vertical
  c.weighty = 1
  c.weightx = 1
  c.gridx = 0
  c.gridy = 1
  c.insets = new Insets(0, 50, 0, 50)
  // Keeping the match explicit here since we do not expect this for Neutral and Below (cards are auto selected)
  val userPromptCardTilePanel = new UserPromptCardTilePanel(app, hand.listOfCards, status match {case President => 2; case VicePres => 1})
  layout(userPromptCardTilePanel) = c

  c.fill = GridBagPanel.Fill.Vertical
  c.weighty = 1
  c.weightx = 1
  c.gridx = 0
  c.gridy = 2
  layout(new Button("Confirm Selection") {
    preferredSize = new Dimension(width, 200)
    font = Font(fontName, Font.Plain, fontSize)
    background = backgroundColor
    opaque = true
    reactions += {
      case ButtonClicked(_) => {
        parent.close()
        selectedCards = userPromptCardTilePanel.selectedCardsQueue.toList
      }
    }
  }) = c

}
