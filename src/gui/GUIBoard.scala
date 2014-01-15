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
import java.awt.event.MouseListener
import java.awt.event.MouseEvent

import src.Cord

class GUIBoard(var board : src.Board, val controler : GUIControler)  
	extends JComponent with MouseListener
{
	var squareLength = 0 // length of one square of chessboard
	var padding = 0 // space for column name and row number

	// sqaures that should be painted in diffrent color
	var activeSquare = 0
	var possbileMovesSquare : List[Int] = Nil
	var attackedSquare = 0

	addMouseListener(this)

	override def paintComponent(graph : Graphics)
	{
		val g = graph.asInstanceOf[Graphics2D]
		val size : Dimension = getSize
		val smallerDimension = math.min(size.height, size.width)
		padding = math.min(smallerDimension / 10, 100)

		// padding on both sides of the chessboard
		squareLength = (smallerDimension -  2 * padding) / 8

		var sqrColorNumber = 0

		g.setRenderingHint(
		        RenderingHints.KEY_TEXT_ANTIALIASING,
				RenderingHints.VALUE_TEXT_ANTIALIAS_ON)

		var y = padding
		var x = 0
		var sqr64Number = 0
		for (row <- 8 to 1 by -1) // blacks on top, so we start from 8 row
		{
			x = padding
			for (column <- 'A' to 'H')
			{
				sqrColorNumber = (sqrColorNumber + 1) % 2
				val sqrColor = GUIBoard.squaresColors(sqrColorNumber)
				g.setColor(sqrColor)

				g.fillRect(x, y, squareLength, squareLength)

				val sqrNumber = Cord.fromString(column.toString + row)
				if (!board.isEmpty(sqrNumber))
				{
					val piece = board.piecesList(board.board(sqrNumber))
					val pieceImage = GUIBoard.piecesImages(piece.color)(piece.pieceType)
					g.drawImage(pieceImage, x, y, squareLength, squareLength, 
						sqrColor, null)
				}
				x += squareLength

			}
			y += squareLength
			sqrColorNumber = (sqrColorNumber + 1) % 2
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

	def showEndGame(gameResult : Int) = 
	{
		val message = gameResult match
		{
			case GUIControler.BLACK_WON => "Black won"
			case GUIControler.WHITE_WON => "White won"
			case GUIControler.DRAW => "Draw"
		}
		JOptionPane.showMessageDialog(null,
		    "Game Over\n" + message)
	}

	override def repaint = 
	{
		val dimension = getSize(null)
		repaint(0, 0, dimension.width, dimension.height)
	}

	override def mouseClicked(e : MouseEvent) = 
	{
		var x = e.getX - padding
		var y = e.getY - padding

		// if we click on chessboard, not on top and leftmargin
		if (x > 0 && y > 0)
		{
			// remeber in our board A1 is index 0, and on the screen it is 
			// painted on the bottom, on 7th row, so we have to adjust
			val row = 7 - y / squareLength
			val column = x / squareLength
			if (row >= 0 && column < 8)
			{
				val sqrNumber64 = row * 8 + column
				val sqr120Number = Cord.from64to120(sqrNumber64)
				controler.fieldClickedEvent(sqr120Number)
			}
		}

	}

	override def mouseEntered(e : MouseEvent) = ()
	override def mouseExited(e : MouseEvent) = ()
	override def mousePressed(e : MouseEvent) = ()
	override def mouseReleased(e : MouseEvent) = ()
}

object GUIBoard
{
	val maxPadding = 30

	final val kit = Toolkit.getDefaultToolkit

	// stores images of pieces
	// piecesImages(color)(pieceType)
	final val piecesImages = Array(
		Array(
			kit.getImage("img/blackPawn.png"),
			kit.getImage("img/blackKnight.png"),
			kit.getImage("img/blackBishop.png"),
			kit.getImage("img/blackRook.png"),
			kit.getImage("img/blackQueen.png"),
			kit.getImage("img/blackKing.png")
		),
		Array(kit.getImage("img/whitePawn.png"),
			kit.getImage("img/whiteKnight.png"),
			kit.getImage("img/whiteBishop.png"),
			kit.getImage("img/whiteRook.png"),
			kit.getImage("img/whiteQueen.png"),
			kit.getImage("img/whiteKing.png")
		)
	)

	val darkSquareColor = Color.gray
	val brightSquareColor = Color.white

	val squaresColors = Array(darkSquareColor, brightSquareColor)
}

