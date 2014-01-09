package src.gui

import src.Board
import src.Cord
import src.Move

class GUIControler
{
	// gui view displaying chessboard
	var view : GUIBoard = null
	// engine class representing chessboard
	var board : src.Board = null

	// square on board actived by previous click
	var activeSqr120 : Int = 0
	var activePieceID : Int = -1

	// function called on each click on square on chess board
	def fieldClickedEvent(sqrNumber120 : Int) = 
	{
		// if we have square on which pieces is present active
		if (board.isOccupied(activeSqr120))
		{
			if (activeSqr120 != sqrNumber120)
			{
				val moves = new Array[Move](1024)
				val index = 0
				board.piecesList(activePieceID).generateMoves(board, moves, index)
				val desiredMove = moves.filter((m : Move) =>
					m != null && m.end == sqrNumber120)
				if (desiredMove.size != 0)
				{
					board.makeMove(desiredMove.head)
					activeSqr120 = 0
				}
				else if (board.isOccupiedByMe(sqrNumber120, board.whoseMove))
				{
					activeSqr120 = sqrNumber120
					activePieceID = board.board(sqrNumber120)
				}
			}
		}
		else
		{
			if (board.isOccupiedByMe(sqrNumber120, board.whoseMove))
			{
				activeSqr120 = sqrNumber120
				activePieceID = board.board(sqrNumber120)
			}
		}
		val dimension = view.getSize(null)
		view.repaint(0, 0, dimension.width, dimension.height)
	}

}
