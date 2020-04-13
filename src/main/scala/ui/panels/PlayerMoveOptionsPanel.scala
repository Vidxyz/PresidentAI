package ui.panels

import java.awt.{Color, Dimension}

import ui.layouts.BottomLayout

import scala.swing.event.ButtonClicked
import scala.swing.{BorderPanel, Button, Swing}

class PlayerMoveOptionsPanel(parent: BottomLayout) extends BorderPanel {

  import PlayerMoveOptionsPanel._

  preferredSize = new Dimension(width, height)
  minimumSize = new Dimension(width, height)
  border = Swing.LineBorder(Color.BLACK)
  background = Color.white

  layout(new Button("Hint") {
    preferredSize = new Dimension(width, height/3)
    reactions += {
      case ButtonClicked(_) => if(parent.getIfRealPlayerTurn) parent.highlightPossibleCards
    }
  }) = BorderPanel.Position.North

  layout(new Button("Pass") {
    preferredSize = new Dimension(width, height/3)
    reactions += {
      case ButtonClicked(_) => if(parent.getIfRealPlayerTurn) parent.updateInternalMoveAsUserPass
    }
  }) = BorderPanel.Position.Center

  layout(new Button("Play") {
    preferredSize = new Dimension(width, height/3)
    reactions += {
      case ButtonClicked(_) => if(parent.getIfRealPlayerTurn) parent.updateInternalMoveUsingSelectedCards
    }
  }) = BorderPanel.Position.South
}

object PlayerMoveOptionsPanel {
  val width = 300
  val height = 300
}
