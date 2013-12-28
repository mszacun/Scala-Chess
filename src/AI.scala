package src

class AI
{
	val max_depth = 5

	def findNextMove(b : Board) = minimax(b, false, max_depth)

	// returns move, and it's score
	def minimax(board : Board, max : Boolean, depth : Int) : (Int, Move) = 
	{
		if (depth > 0)
		{
			val whoseKing = board.whoseMove ^ 1
			val kingToCheckID = if (whoseKing == Piece.WHITE) Board.WHITE_KING
							else                          Board.BLACK_KING
			val pos = board.piecesList(kingToCheckID).position
			if (!board.isAttacked(pos, whoseKing))
			{
				if (max)
				{
					var result = Integer.MIN_VALUE
							var choosenMove : Move = null
							val (moves, size) = board.generateMovesForNextPlayer
							var i : Int = 0
							while (i < size)
							{
								board.makeMove(moves(i))
								val moveScore = minimax(board, !max, depth - 1)._1
								if (moveScore > result)
								{
									result = moveScore
											choosenMove = moves(i)
								}
								board.undoMove
								i += 1
							}
					(result, choosenMove)
				}
				else
				{
					var result = Integer.MAX_VALUE
					var choosenMove : Move = null
					val (moves, size) = board.generateMovesForNextPlayer
					var i : Int = 0
					while (i < size)
					{
						board.makeMove(moves(i))
						val moveScore = minimax(board, !max, depth - 1)._1
						if (moveScore < result)
						{
							result = moveScore
							choosenMove = moves(i)
						}
						board.undoMove
						i += 1
					}
					(result, choosenMove)
				}
			}
			else if (max) (Integer.MAX_VALUE, null) else (Integer.MIN_VALUE, null)
		}
		else
			(board.getPlayerScore(board.whoseMove), null)
	}

}
 