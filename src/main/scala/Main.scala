import ui.MainLayout
import ui.layouts.MenuBarLayout

import scala.swing.{Frame, MainFrame, SimpleSwingApplication}

object Main extends SimpleSwingApplication {

  lazy val mainLayout: MainLayout = new MainLayout(this)

  def top: Frame = new MainFrame {
    title = "President Card Game"
    menuBar = new MenuBarLayout(mainLayout)
    contents = mainLayout
    resizable = true
  }

}




