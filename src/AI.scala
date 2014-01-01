package src

class AI
{
	val maxDepth = 50

	var actualDepth = 4
	var nodesVisited : Long = 0
	var pvMoves = new Array[Move](maxDepth)

	def findNextMove(b : Board, opponent : Int) : (Int, List[Move]) = 
	{
		nodesVisited = 0
		alphabeta(b, false, 0, Integer.MIN_VALUE,
			Integer.MAX_VALUE, opponent)
	}

	// returns move, and it's score
	def alphabeta(board : Board, max : Boolean, depth : Int, alp : Int,
		bet : Int, op : Int) : (Int, List[Move]) = 
	{
		var alpha = alp
		var beta = bet
		if (depth < actualDepth)
		{
			nodesVisited += 1
			if (max)
			{
				var choosenPath : List[Move] = Nil
				val (moves, size) = board.generateMovesForNextPlayer
				var i : Int = 0
				var validMoves = 0 // counts how many valid moves are possible
				while (i < size)
				{
					val move = moves(i)
					if (board.makeMove(move))
					{
						validMoves += 1
						val childMove  = alphabeta(board, !max, depth + 1, alpha, beta, op)
						val moveScore = childMove._1
						if (moveScore > alpha)
						{
							alpha = moveScore
							choosenPath = move :: childMove._2
							if (beta <= alpha)
							{
								board.undoMove
								return (alpha, choosenPath)
							}
						}
					}
					board.undoMove
					i += 1
				}
				if (validMoves > 0)
					return (alpha, choosenPath)
				else 
					if (board.isCheck(board.whoseMove))
					{
						if (board.whoseMove == op) (Integer.MIN_VALUE + 1, Nil) 
						   	else (Integer.MAX_VALUE - 1, Nil)
					}
					else
						(0, Nil)
			}
			else
			{
				var choosenPath : List[Move] = Nil
				val (moves, size) = board.generateMovesForNextPlayer
				var i : Int = 0
				var validMoves = 0
				while (i < size)
				{
					val move = moves(i)
					if (board.makeMove(move))
					{
						validMoves += 1
						val childMove  = alphabeta(board, !max, depth + 1, alpha, beta, op)
						val moveScore = childMove._1
						if (moveScore < beta)
						{
							beta = moveScore
							choosenPath = move :: childMove._2
							if (beta <= alpha)
							{
								board.undoMove
								return (beta, choosenPath)
							}
						}
					}
					board.undoMove
					i += 1
				}
				if (validMoves > 0)
					return (beta, choosenPath)
				else 
					if (board.isCheck(board.whoseMove))
					{
						if (board.whoseMove == op) (Integer.MIN_VALUE + 1, Nil) 
						   	else (Integer.MAX_VALUE - 1, Nil)
					}
					else
						(0, Nil)
			}
		}
		else
		{
//			(board.getPlayerScore(op), Nil)
			quiescence(board, max, alp, bet, op)
		}
	}
	
	def quiescence(board : Board, max : Boolean, alp : Int,
		bet : Int, op : Int) : (Int, List[Move]) = 
	{
		nodesVisited += 1
		var alpha = alp
		var beta = bet
		val score = board.getPlayerScore(op)
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
			var validMoves = 0 // counts how many valid moves are possible
			while (i < size)
			{
				val move = moves(i)
				if (board.makeMove(move))
				{
					validMoves += 1
					val childMove  = quiescence(board, !max, alpha, beta, op)
					val moveScore = childMove._1
					if (moveScore > alpha)
					{
						alpha = moveScore
						choosenPath = move :: childMove._2
						if (beta <= alpha)
						{
							board.undoMove
							return (alpha, choosenPath)
						}
					}
				}
				board.undoMove
				i += 1
			}
			if (validMoves > 0 || size == 0)
				return (alpha, choosenPath)
			else 
				if (board.isCheck(board.whoseMove))
				{
					if (board.whoseMove == op) (Integer.MIN_VALUE + 1, Nil) 
					   	else (Integer.MAX_VALUE - 1, Nil)
				}
				else
					(0, Nil)
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
			var validMoves = 0
			while (i < size)
			{
				val move = moves(i)
				if (board.makeMove(move))
				{
					validMoves += 1
					val childMove  = quiescence(board, !max, alpha, beta, op)
					val moveScore = childMove._1
					if (moveScore < beta)
					{
						beta = moveScore
						choosenPath = move :: childMove._2
						if (beta <= alpha)
						{
							board.undoMove
							return (beta, choosenPath)
						}
					}
				}
				board.undoMove
				i += 1
			}
			if (validMoves > 0 || size == 0)
				return (beta, choosenPath)
			else 
				if (board.isCheck(board.whoseMove))
				{
					if (board.whoseMove == op) (Integer.MIN_VALUE + 1, Nil) 
					   	else (Integer.MAX_VALUE - 1, Nil)
				}
				else
					(0, Nil)
		}
	}
}
 
