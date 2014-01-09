package src.gui

import javax.swing._
import java.awt.Color
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.Toolkit
import java.awt.Dimension
import javax.swing.JComponent

object Main extends App
{
	val frame = new JFrame("Chess")
	val startFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
	val board = src.Board(startFEN)
	val controller = new GUIControler
	val view = new GUIBoard(board, controller)
	frame.setVisible(true)
	frame.setSize(400, 400)
	frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
	frame.add(view)
	controller.view = view
	controller.board = board
}

