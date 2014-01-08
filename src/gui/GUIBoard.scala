package src.gui

import javax.swing._
import java.awt.Color
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.Toolkit
import java.awt.Dimension
import javax.swing.JComponent
import java.awt.Font
import java.awt.RenderingHints

import src.Cord

class GUIBoard(val board : src.Board)  extends JComponent
{
	var squareLength = 0

	override def paintComponent(graph : Graphics)
	{
		val g = graph.asInstanceOf[Graphics2D]
		val size : Dimension = getSize
		val smallerDimension = math.min(size.height, size.width)
		// space for column name and row number
		val padding = math.min(smallerDimension / 10, 100)

		// padding on both sides of the chessboard
		squareLength = (smallerDimension -  2 * padding) / 8

		var squareColor = GUIBoard.darkSquareColor

		g.setRenderingHint(
		        RenderingHints.KEY_TEXT_ANTIALIASING,
				RenderingHints.VALUE_TEXT_ANTIALIAS_ON)

		var y = padding
		var x = 0
		var sqr64Number = 0
		for (row <- 1 to 8)
		{
			x = padding
			for (column <- 'A' to 'H')
			{
				if (squareColor == GUIBoard.darkSquareColor)
					squareColor = GUIBoard.brightSquareColor
				else
					squareColor = GUIBoard.darkSquareColor
				g.setColor(squareColor)

				g.fillRect(x, y, squareLength, squareLength)

				val sqrNumber = Cord.from64to120(sqr64Number)
				if (!board.isEmpty(sqrNumber))
				{
					val piece = board.piecesList(board.board(sqrNumber))
					val pieceImage = GUIBoard.piecesImages(piece.color)(piece.pieceType)
					g.drawImage(pieceImage, x, y, squareLength, squareLength, squareColor, null)
				}
				x += squareLength
				sqr64Number += 1
			}
			y += squareLength
			if (squareColor == GUIBoard.darkSquareColor)
				squareColor = GUIBoard.brightSquareColor
			else
				squareColor = GUIBoard.darkSquareColor
		}

		g.setColor(Color.black)
		g.setFont(new Font("Times New Roman", Font.PLAIN, math.max(padding / 3, 10)))

		// column names 
		x = padding + squareLength / 2
		for (column <- 'A' to 'H')
		{
			g.drawString(column.toString, x, padding / 2) // top
			// bottom
			g.drawString(column.toString, x, padding + squareLength * 8 + padding / 2)
			x += squareLength
		}
		// row number
		y = padding + squareLength / 2
		for (row <- 8 to 1 by -1)
		{
			// left
			g.drawString(row.toString, padding / 2, y)
			// right
			g.drawString(row.toString, padding + squareLength * 8 + padding / 2, y)
			y += squareLength
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

	val darkSquareColor = Color.gray
	val brightSquareColor = Color.white
}

