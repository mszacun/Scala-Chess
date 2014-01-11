package src.gui

import javax.swing.Timer
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import scala.swing._

import src.Board
import src.Cord
import src.Move
import src.AI
import src.Piece

class GUIControler
{
	// gui view displaying chessboard
	var view : GUIBoard = null
	// engine class representing chessboard
	var board : src.Board = null

	// square on board actived by previous click
	var activeSqr120 : Int = 0

	val ai = new AI(Piece.WHITE)
	val thinkingTime = 5 * 1000

	// timer used to give time for repaint, before computer starts thinking
	val computerThinkTimer = new Timer(100, Swing.ActionListener(e => 
	{
		val dimension = view.getSize(null)
		val (score, bestPath) = ai.findNextMove(board, thinkingTime)
		if (bestPath != Nil)
		{
			println(bestPath + " score: " + score)
			board.makeMove(bestPath.head)
			view.repaint
		}
		else
			println("Game OVER: " + score)
	}))
	computerThinkTimer.setRepeats(false)


	// function called on each click on square on chess board
	def fieldClickedEvent(sqrNumber120 : Int) = 
	{
		val dimension = view.getSize(null)
		// if we have square on which pieces is present active
		if (board.isOccupied(activeSqr120))
		{
			if (activeSqr120 != sqrNumber120)
			{
				if (!tryMakeMove(sqrNumber120) && 
					board.isOccupiedByMe(sqrNumber120, board.whoseMove))
				{
					activeSqr120 = sqrNumber120
				}
			}
		}
		else
		{
			if (board.isOccupiedByMe(sqrNumber120, board.whoseMove))
			{
				activeSqr120 = sqrNumber120
			}
		}
	}

	// tries to move active piece to clicked square, returns if it was possible
	def tryMakeMove(targetSquare120 : Int) : Boolean = 
	{
		val dimension = view.getSize(null)
		val (moves, size) = board.generateMovesForNextPlayer

		val desiredMove = moves.filter((m : Move) =>
			m != null && m.end == targetSquare120 && m.start == activeSqr120)

		if (desiredMove.size != 0)
		{
			if (board.makeMove(desiredMove.head))
			{
				view.repaint
				activeSqr120 = 0

				// make computer move
				computerThinkTimer.start()
			}
			else
				board.undoMove

			return true
		}
		else
			return false
	}

}
