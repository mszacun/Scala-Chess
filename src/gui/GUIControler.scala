package src.gui

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

	// function called on each click on square on chess board
	def fieldClickedEvent(sqrNumber120 : Int) = 
	{
		val dimension = view.getSize(null)
		// if we have square on which pieces is present active
		if (board.isOccupied(activeSqr120))
		{
			if (activeSqr120 != sqrNumber120)
			{
				val (moves, size) = board.generateMovesForNextPlayer
				val desiredMove = moves.filter((m : Move) =>
					m != null && m.end == sqrNumber120 && m.start == activeSqr120)
				if (desiredMove.size != 0)
				{
					if (board.makeMove(desiredMove.head))
					{
						activeSqr120 = 0

						// make computer move
						// TODO: Move it to other thread
						val (score, bestPath) = ai.findNextMove(board, thinkingTime)
						if (bestPath != Nil)
						{
							println(bestPath + " score: " + score)
							board.makeMove(bestPath.head)
						}
						else
							println("Game OVER: " + score)
					}
					else
						board.undoMove
				}
				else if (board.isOccupiedByMe(sqrNumber120, board.whoseMove))
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
		view.repaint(0, 0, dimension.width, dimension.height)
	}

}
