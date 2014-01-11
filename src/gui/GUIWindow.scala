package src.gui

import javax.swing._
import javax.swing.JComponent
import javax.swing.JMenuBar


class GUIWindow extends JFrame("Chess")
{
	val startFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
	val board = src.Board(startFEN)
	val controller = new GUIControler
	val view = new GUIBoard(board, controller)

	// create menu
	val theMenuBar = new JMenuBar

	val menuMenu = new JMenu("Menu")

	val newGameMenu = new JMenu("New game")
	val humanComputerMenuItem = new JMenuItem("Human vs Computer")
	val computerHumanMenuItem = new JMenuItem("Computer vs Human")
	val computerComputerMenuItem = new JMenuItem("Computer vs Computer")

	newGameMenu.add(humanComputerMenuItem)
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
}
