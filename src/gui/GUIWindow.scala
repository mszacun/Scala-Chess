package src.gui

import javax.swing._
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import javax.swing.JComponent
import javax.swing.JMenuBar


class GUIWindow extends JFrame("Chess") with ActionListener
{
	val board = src.Board(GUIControler.startFEN)
	val controller = new GUIControler
	val view = new GUIBoard(board, controller)

	// create menu
	val theMenuBar = new JMenuBar

	val menuMenu = new JMenu("Menu")

	val newGameMenu = new JMenu("New game")

	val humanComputerMenuItem = new JMenuItem("Human vs Computer")
	humanComputerMenuItem.addActionListener(this)

	val computerHumanMenuItem = new JMenuItem("Computer vs Human")
	computerHumanMenuItem.addActionListener(this)

	val computerComputerMenuItem = new JMenuItem("Computer vs Computer")
	computerComputerMenuItem.addActionListener(this)

	newGameMenu.add(humanComputerMenuItem)
	newGameMenu.add(computerHumanMenuItem)
	newGameMenu.add(computerComputerMenuItem)

	menuMenu.add(newGameMenu)

	theMenuBar.add(menuMenu)

	setJMenuBar(theMenuBar)

	setVisible(true)
	setSize(400, 400)
	setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
	add(view)
	controller.view = view
	controller.board = board

	override def actionPerformed(event : ActionEvent) =
	{
		event.getSource match
		{
			case `humanComputerMenuItem` => controller.startNewGamePVC
			case `computerHumanMenuItem` => controller.startNewGameCVP
			case `computerComputerMenuItem` => controller.startNewGameCVC
		}
	}
}
