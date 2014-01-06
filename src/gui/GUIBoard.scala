package src.gui

import javax.swing._
import java.awt.Color
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.Toolkit
import java.awt.Dimension
import javax.swing.JComponent

import src.Cord

class GUIBoard(val board : src.Board)  extends JComponent
{

	override def paintComponent(graph : Graphics)
	{
		val g = graph.asInstanceOf[Graphics2D]
		val size : Dimension = getSize

		val squareLength = math.min(size.height, size.width) / 8

		var squareColor = GUIBoard.darkSquareColor

		var y = 0
		for (row <- 1 to 8)
		{
			var x = 0
			for (column <- 'A' to 'H')
			{
				if (squareColor == GUIBoard.darkSquareColor)
					squareColor = GUIBoard.brightSquareColor
				else
					squareColor = GUIBoard.darkSquareColor
				g.setColor(squareColor)

				g.fillRect(x, y, squareLength, squareLength)

				val sqrNumber = Cord.fromString(column.toString + row)
				if (!board.isEmpty(sqrNumber))
				{
					val piece = board.piecesList(board.board(sqrNumber))
					val pieceImage = GUIBoard.piecesImages(piece.color)(piece.pieceType)
					g.drawImage(pieceImage, x, y, squareLength, squareLength, squareColor, null)
				}
				x += squareLength
			}
			y += squareLength
			if (squareColor == GUIBoard.darkSquareColor)
				squareColor = GUIBoard.brightSquareColor
			else
				squareColor = GUIBoard.darkSquareColor
		}
	}
}

object GUIBoard
{
	val maxPadding = 30

	final val kit = Toolkit.getDefaultToolkit

	// stores images of pieces
	// piecesImages(color)(pieceType)
	final val piecesImages = Array(
		Array(kit.getImage("img/whitePawn.png"),
			kit.getImage("img/whiteKnight.png"),
			kit.getImage("img/whiteBishop.png"),
			kit.getImage("img/whiteRook.png"),
			kit.getImage("img/whiteQueen.png"),
			kit.getImage("img/whiteKing.png")
		),
		Array(
			kit.getImage("img/blackPawn.png"),
			kit.getImage("img/blackKnight.png"),
			kit.getImage("img/blackBishop.png"),
			kit.getImage("img/blackRook.png"),
			kit.getImage("img/blackQueen.png"),
			kit.getImage("img/blackKing.png")
		)
	)

	val darkSquareColor = Color.blue
	val brightSquareColor = Color.white
}

