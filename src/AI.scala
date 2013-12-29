package src

class AI
{
	val max_depth = 7

	def findNextMove(b : Board, opponent : Int) = 
		alphabeta(b, false, max_depth, Integer.MIN_VALUE,
			Integer.MAX_VALUE, opponent)

	// returns move, and it's score
	def alphabeta(board : Board, max : Boolean, depth : Int, alp : Int,
		bet : Int, op : Int) : (Int, List[Move]) = 
	{
		var alpha = alp
		var beta = bet
		if (depth > 0)
		{
			// check if this is legal position
			val whoseKing = board.whoseMove ^ 1
			val kingToCheckID = if (whoseKing == Piece.WHITE) Board.WHITE_KING
								else                          Board.BLACK_KING
			val pos = board.piecesList(kingToCheckID).position
			if (!board.isAttacked(pos, whoseKing))
			{
				if (max)
				{
					var choosenPath : List[Move] = Nil
					val (moves, size) = board.generateMovesForNextPlayer
					var i : Int = 0
					while (i < size)
					{
						board.makeMove(moves(i))
						val childMove  = alphabeta(board, !max, depth - 1, alpha, beta, op)
						val moveScore = childMove._1
						board.undoMove
						if (moveScore > alpha)
						{
							alpha = moveScore
							choosenPath = moves(i) :: childMove._2
							if (beta <= alpha)
								return (alpha, choosenPath)
						}
						i += 1
					}
					(alpha, choosenPath)
				}
				else
				{
					var choosenPath : List[Move] = Nil
					val (moves, size) = board.generateMovesForNextPlayer
					var i : Int = 0
					while (i < size)
					{
						board.makeMove(moves(i))
						val childMove  = alphabeta(board, !max, depth - 1, alpha, beta, op)
						val moveScore = childMove._1
						board.undoMove
						if (moveScore < beta)
						{
							beta = moveScore
							choosenPath = moves(i) :: childMove._2
							if (beta <= alpha)
								return (beta, choosenPath)
						}
						i += 1
					}
					(beta, choosenPath)
				}
			}
			else if (max) (Integer.MAX_VALUE, Nil) else (Integer.MIN_VALUE, Nil)
		}
		else
			(board.getPlayerScore(op), Nil)
	}
}
 
