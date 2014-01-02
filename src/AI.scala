package src

import scala.util.Sorting

class AI
{
	val maxDepth = 500

	var actualDepth = 5
	var nodesVisited : Long = 0
	var allNodesVisited : Long = 0
	var pvMoves = new Array[Move](maxDepth)
	var stopTime : Long = 0 // time limit for next search

	object MoveOrdering extends Ordering[Move]
	{
		def compare(a : Move, b : Move) = 
		{
			val score1 = if (a == null) Integer.MIN_VALUE else a.score
			val score2 = if (b == null) Integer.MIN_VALUE else b.score
			score2 compare score1
		}
	}

	// time -> time for searchin in miliseconds
	def findNextMove(b : Board, opponent : Int, time : Int) : (Int, List[Move]) = 
	{
		nodesVisited = 0
		stopTime = System.currentTimeMillis + time
		var lastResult : (Int, List[Move]) = (0, Nil)

		for (i <- 0 until maxDepth)
			pvMoves(i) = null

		for (i <- 1 to maxDepth)
		{
			actualDepth = i
			val currentResult = alphabeta(b, false, 0, Integer.MIN_VALUE,
				Integer.MAX_VALUE, opponent)
			allNodesVisited += nodesVisited

			// if time is up, alphaBeta returns Nil path
			if (currentResult._2 == Nil)
				return lastResult
			else
				lastResult = currentResult
			nodesVisited = 0
		}
		return lastResult
	}

	def orderMoves(moves : Array[Move], size : Int, b : Board, depth : Int)
	{
		var i = 0
		while (i < size)
		{
			val move = moves(i)
			if (move equals pvMoves(depth))
			{
				move.score = Integer.MAX_VALUE
			}
			else
				move.calculateScore(b)
			i += 1
		}
		Sorting.quickSort(moves)(MoveOrdering)
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
			if (board.countRepetitions >= 3) // threefold repetition
				return (0, Nil)
			if (max)
			{
				var choosenPath : List[Move] = Nil
				var (moves, size) = board.generateMovesForNextPlayer
				var i : Int = 0
				var validMoves = 0 // counts how many valid moves are possible

				orderMoves(moves, size, board, depth)
				while (i < size)
				{
					if (System.currentTimeMillis > stopTime) // time control
						return (0, Nil)

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
				{
					if (alpha > alp) // store new pv move
						pvMoves(depth) = choosenPath.head
					return (alpha, choosenPath)
				}
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

				orderMoves(moves, size, board, depth)
				while (i < size)
				{
					if (System.currentTimeMillis > stopTime) // time control
						return (0, Nil)

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
				{
					if (beta < bet)
						pvMoves(depth) = choosenPath.head // store pv node
					return (beta, choosenPath)
				}
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
			quiescence(board, max, alp, bet, op)
	}
	
	def quiescence(board : Board, max : Boolean, alp : Int,
		bet : Int, op : Int) : (Int, List[Move]) = 
	{
		var alpha = alp
		var beta = bet
		nodesVisited += 1
		if (board.countRepetitions >= 3) // threefold repetition
			return (0, Nil)
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

			// quiescence shouldnt use pvMoves, cause it searches only attacks
			orderMoves(moves, size, board, 0) 
			var i : Int = 0
			var validMoves = 0 // counts how many valid moves are possible
			while (i < size)
			{
				if (System.currentTimeMillis > stopTime) // time control
					return (0, Nil)

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

			// quiescence shouldnt use pvMoves, cause it searches only attacks
			orderMoves(moves, size, board, 0)
			while (i < size)
			{
				if (System.currentTimeMillis > stopTime) // time control
					return (0, Nil)

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
 
