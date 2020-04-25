package ui.layouts

import java.awt.Toolkit

import javax.swing.KeyStroke
import ui.MainLayout

import scala.swing.{Action, Menu, MenuBar, MenuItem, Separator}
import scala.swing.Dialog._

class MenuBarLayout(mainLayout: MainLayout) extends MenuBar {

  val newGameAction: Action = Action("New Game") {
    showConfirmation(mainLayout, "Are you sure? You will lose all game progress!", "New Game")  match {
      case Result.Yes => mainLayout.promptDialogForNewGame
      case Result.No  =>
    }
  }
  newGameAction.accelerator = Some(KeyStroke.getKeyStroke('N', Toolkit.getDefaultToolkit.getMenuShortcutKeyMask))

  val reDealAction: Action = Action("Re-Deal Hands") {
    showConfirmation(mainLayout, "Are you sure? All hands will be re-dealt and you will lose current game progress!", "Re-Deal Hands") match {
      case Result.Yes => mainLayout.reDealHandsForThisGame
      case Result.No  =>
    }
  }
  reDealAction.accelerator = Some(KeyStroke.getKeyStroke('R', Toolkit.getDefaultToolkit.getMenuShortcutKeyMask))

  val quitAction = Action("Quit") {
    showConfirmation(mainLayout, "Are you sure? This window will close and you will lose all progress!", "Quit") match {
      case Result.Yes => System.exit(0)
      case Result.No  =>
    }
  }
  quitAction.accelerator = Some(KeyStroke.getKeyStroke('Q', Toolkit.getDefaultToolkit.getMenuShortcutKeyMask))

  contents += new Menu("Menu") {
    contents += new MenuItem(newGameAction)
    contents += new MenuItem(reDealAction)
    contents += new Separator
    contents += new MenuItem(quitAction)
  }
}
