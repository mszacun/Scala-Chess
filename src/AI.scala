package src

class AI
{
	val max_depth = 5

	def findNextMove(b : Board, opponent : Int) : (Int, List[Move]) = 
		alphabeta(b, false, max_depth, Integer.MIN_VALUE,
			Integer.MAX_VALUE, opponent)

	// returns move, and it's score
	def alphabeta(board : Board, max : Boolean, depth : Int, alp : Int,
		bet : Int, op : Int) : (Int, List[Move]) = 
	{
		var alpha = alp
		var beta = bet
		// check if this is legal position
		val whoseKing = board.whoseMove ^ 1
		val kingToCheckID = if (whoseKing == Piece.WHITE) Board.WHITE_KING
							else                          Board.BLACK_KING
		val pos = board.piecesList(kingToCheckID).position
		if (!board.isAttacked(pos, whoseKing))
		{
			if (depth > 0)
			{
				if (max)
				{
					var choosenPath : List[Move] = Nil
					val (moves, size) = board.generateMovesForNextPlayer
					var i : Int = 0
					while (i < size)
					{
						val move = moves(i)
						board.makeMove(move)
						val childMove  = alphabeta(board, !max, depth - 1, alpha, beta, op)
						board.undoMove
						val moveScore = childMove._1
						if (moveScore > alpha)
						{
							alpha = moveScore
							choosenPath = move :: childMove._2
							if (beta <= alpha)
								return (alpha, choosenPath)
						}
						i += 1
					}
					return (alpha, choosenPath)
				}
				else
				{
					var choosenPath : List[Move] = Nil
					val (moves, size) = board.generateMovesForNextPlayer
					var i : Int = 0
					while (i < size)
					{
						val move = moves(i)
						board.makeMove(move)
						val childMove  = alphabeta(board, !max, depth - 1, alpha, beta, op)
						board.undoMove
						val moveScore = childMove._1
						if (moveScore < beta)
						{
							beta = moveScore
							choosenPath = move :: childMove._2
							if (beta <= alpha)
								return (beta, choosenPath)
						}
						i += 1
					}
						return (beta, choosenPath)
				}
			}
			else
			{
	//			(board.getPlayerScore(op), Nil)
				(quiescence(board, max, alp, bet, op)._1, Nil)
			}
		}
		else
		{
				return if (whoseKing == op) (Integer.MIN_VALUE, Nil) else (Integer.MAX_VALUE, Nil)
		}
	}
	
def quiescence(board : Board, max : Boolean, alp : Int,
	bet : Int, op : Int) : (Int, List[Move]) = 
	{
		var alpha = alp
		var beta = bet

		// check if this is legal position
		val score = board.getPlayerScore(op)
		val whoseKing = board.whoseMove ^ 1
		val kingToCheckID = if (whoseKing == Piece.WHITE) Board.WHITE_KING
							else                          Board.BLACK_KING
		val pos = board.piecesList(kingToCheckID).position
		if (!board.isAttacked(pos, whoseKing))
		{
			if (max)
			{
				if (score > alpha)
				{
					alpha = score
					if (beta <= alpha)
						return (alpha, null)
				}
				var choosenPath : List[Move] = Nil
				val (moves, size) = board.generateAttacksForNextPlayer
				var i : Int = 0
				while (i < size)
				{
					board.makeMove(moves(i))
					val childMove  = quiescence(board, !max, alpha, beta, op)
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
				if (score < beta)
				{
					beta = score
					if (beta <= alpha)
						return (beta, null)
				}
				var choosenPath : List[Move] = Nil
				val (moves, size) = board.generateAttacksForNextPlayer
				var i : Int = 0
				while (i < size)
				{
					board.makeMove(moves(i))
					val childMove  = quiescence(board, !max, alpha, beta, op)
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
		else
		{
			return if (whoseKing == op) (Integer.MIN_VALUE, Nil) else (Integer.MAX_VALUE, Nil)
		}
	}

}
 
