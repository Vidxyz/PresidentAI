package ui.layouts

import ui.MainLayout

import scala.swing.{Action, Menu, MenuBar, MenuItem, Separator}
import scala.swing.Dialog._

class MenuBarLayout(mainLayout: MainLayout) extends MenuBar {

  contents += new Menu("Menu") {
    contents += new MenuItem(Action("New Game") {
      showConfirmation(mainLayout, "Are you sure? You will lose all game progress", "Confirm Selection")  match {
        case Result.Yes => println("Selected yes")
        case Result.No  => println("Selected no")
      }
    })
    contents += new Separator
    contents += new MenuItem(Action("Restart") {
      showConfirmation(mainLayout, "Are you sure? All hands will be re-dealt and you will lose all game progress", "Confirm Selection") match {
        case Result.Yes => println("Selected yes")
        case Result.No  => println("Selected no")
      }
    })
  }
}
