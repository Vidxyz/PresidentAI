package ui.layouts

import ui.MainLayout

import scala.swing.{Action, Menu, MenuBar, MenuItem, Separator}
import scala.swing.Dialog._

class MenuBarLayout(mainLayout: MainLayout) extends MenuBar {

  contents += new Menu("Menu") {
    contents += new MenuItem(Action("New Game") {
      showConfirmation(mainLayout, "Are you sure? You will lose all game progress!", "Confirm Selection")  match {
        case Result.Yes => mainLayout.promptDialogForNewGame
        case Result.No  =>
      }
    })
    contents += new MenuItem(Action("Re-Deal Hands") {
      showConfirmation(mainLayout, "Are you sure? All hands will be re-dealt and you will lose all game progress!", "Confirm Selection") match {
        case Result.Yes => mainLayout.reDealHandsForThisGame
        case Result.No  =>
      }
    })
    contents += new Separator
    contents += new MenuItem(Action("Quit") {
      showConfirmation(mainLayout, "Are you sure? This window will close and you will lose all progress!", "Confirm Selection") match {
        case Result.Yes => System.exit(0)
        case Result.No  =>
      }
    })
  }
}
