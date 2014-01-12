package src

import scala.util.Sorting

class AI(val opponent : Int)
{
	val maxDepth = 500
	val transpositionTableSize = 1024 * 1024 * 5 // 5 Mb
	val killersTabSize = 3
	val aspirationWindowWidth = 50 // calculated experimentally

	var actualDepth = 0
	var nodesVisited : Long = 0 // nodes visted on current depth
	var allNodesVisited : Long = 0 // nodes visited during whole search
	var transpositionTable = new TranspositionTable(transpositionTableSize)
	var stopTime : Long = 0 // time limit for next search
	var killers = new Array[Array[Move]](maxDepth) // stores moves that caused cut-off

	// initialize killers
	for (i <- 0 until maxDepth)
		killers(i) = new Array[Move](killersTabSize)

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
	def findNextMove(b : Board, time : Int) : (Int, List[Move]) = 
	{
		nodesVisited = 0
		var alpha = Integer.MIN_VALUE
		var beta = Integer.MAX_VALUE
		stopTime = System.currentTimeMillis + time
		var lastResult : (Int, List[Move]) = (0, Nil)

		for (i <- 1 to maxDepth)
		{
			actualDepth = i
			var currentResult = alphabeta(b, false, 0, alpha,
				beta)

			// check if we hit into aspiration window
			if (currentResult._1 <= alpha)
				currentResult = alphabeta(b, false, 0, Integer.MIN_VALUE,
					beta)
			else if (currentResult._1 >= beta)
				currentResult = alphabeta(b, false, 0, alpha,
					Integer.MAX_VALUE)

			// calculate next aspiration window
			alpha = currentResult._1 - aspirationWindowWidth
			beta = currentResult._1 + aspirationWindowWidth

			allNodesVisited += nodesVisited
			println("Depth: " + i + " nodes: " + nodesVisited)

			// if time is up, alphaBeta returns Nil path
			if (currentResult._2 == Nil)
				return lastResult
			else
				lastResult = currentResult
			nodesVisited = 0
		}
		return lastResult
	}

	/*    ORDERING RULES:
		* PV-Move is scored best: Integer.MAX_VALUE
		* Captures with promotions are next, scored based on LVA/MVV rule + 20 000
		* Next ordinary captures, scored based on LVA/MVV + 10 000 (enPassant
		*      captures comes here)
		* Next comes killer moves: 1st is scored 8000, 2nd 7000, 3rd 6000
		* Next we order castles: 5000
		* Lastly ordinary quiet moves are scored 0 (planning using history heuristics here)
		*
	*/
	def orderMoves(moves : Array[Move], size : Int, b : Board, usePV : Boolean,
		depth : Int)
	{
		var i = 0
		var path = if (usePV) transpositionTable.getMove(b.boardHash) else Nil
		var pv = if (path != Nil) path.head else null
		while (i < size)
		{
			val move = moves(i)
			if (move equals pv)
			{
				move.score = Integer.MAX_VALUE
			}
			else if (move equals killers(depth)(0))
				move.score = 8000
			else if (move equals killers(depth)(1))
				move.score = 7000
			else if (move equals killers(depth)(2))
				move.score = 6000
			else
				move.calculateScore(b)
			i += 1
		}
		Sorting.quickSort(moves)(MoveOrdering)
	}

	def storeKiller(killer : Move, depth : Int) = 
	{
		if (!killer.isCapture)
		{
			killers(depth)(2) = killers(depth)(1)
			killers(depth)(1) = killers(depth)(0)
			killers(depth)(0) = killer
		}
	}

	// returns move, and it's score
	def alphabeta(board : Board, max : Boolean, depth : Int, alp : Int,
		bet : Int) : (Int, List[Move]) = 
	{
		// in check extension
		if (depth >= actualDepth && !board.isCheck(board.whoseMove))
			return quiescence(board, max, alp, bet, depth + 1)

		nodesVisited += 1

		// if we can force situation two times, we can probably force it 3th time
		// this is necessary because of transposition table
		if (board.countRepetitions >= 2) // threefold repetition
			return (0, Nil)

		val remainingDepth = actualDepth - depth
		val tableScore = transpositionTable.getScore(board.boardHash, 
			remainingDepth, alp, bet)
		if (tableScore != Hash.UNKNOW_VALUE)
			return (tableScore, transpositionTable.getMove(board.boardHash))

		var alpha = alp
		var beta = bet
		var choosenPath : List[Move] = Nil
		var (moves, size) = board.generateMovesForNextPlayer
		var i : Int = 0
		var validMoves = 0 // counts how many valid moves are possible

		orderMoves(moves, size, board, true, depth)
		if (max)
		{
			while (i < size)
			{
				if (System.currentTimeMillis > stopTime) // time control
					return (0, Nil)

				val move = moves(i)
				if (board.makeMove(move))
				{
					validMoves += 1
					val childMove  = alphabeta(board, !max, depth + 1, alpha, beta)
					val moveScore = childMove._1
					if (moveScore > alpha)
					{
						alpha = moveScore
						choosenPath = move :: childMove._2
						if (beta <= alpha)
						{
							board.undoMove
							storeKiller(choosenPath.head, depth)
							transpositionTable.set(board.boardHash, Nil,
								alpha, remainingDepth, Hash.FAIL_HIGH_ALPHA)
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
					transpositionTable.set(board.boardHash, choosenPath,
						alpha, remainingDepth, Hash.EXACT)
				else
					transpositionTable.set(board.boardHash, choosenPath,
						alpha, remainingDepth, Hash.FAIL_LOW_ALPHA)
				return (alpha, choosenPath)
			}
			else 
				if (board.isCheck(board.whoseMove))
				{
					if (board.whoseMove == opponent) (-AI.MATE + depth, Nil) 
					   	else (AI.MATE - depth, Nil)
				}
				else
					(0, Nil)
		}
		else
		{
			while (i < size)
			{
				if (System.currentTimeMillis > stopTime) // time control
					return (0, Nil)

				val move = moves(i)
				if (board.makeMove(move))
				{
					validMoves += 1
					val childMove  = alphabeta(board, !max, depth + 1, alpha, beta)
					val moveScore = childMove._1
					if (moveScore < beta)
					{
						beta = moveScore
						choosenPath = move :: childMove._2
						if (beta <= alpha)
						{
							board.undoMove
							storeKiller(choosenPath.head, depth)
						// FIXME: Those lines causes engine to play weaker
						//	transpositionTable.set(board.boardHash, null,
						//		beta, remainingDepth, Hash.FAIL_LOW_BETA)
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
					transpositionTable.set(board.boardHash, choosenPath,
						beta, remainingDepth, Hash.EXACT)
				else
					transpositionTable.set(board.boardHash, choosenPath,
						beta, remainingDepth, Hash.FAIL_HIGH_BETA)
				return (beta, choosenPath)
			}
			else 
				if (board.isCheck(board.whoseMove))
				{
					if (board.whoseMove == opponent) (-AI.MATE + depth, Nil) 
					   	else (AI.MATE - depth, Nil)
				}
				else
					(0, Nil)
		}
	}

	def quiescence(board : Board, max : Boolean, alp : Int,
		bet : Int, depth : Int) : (Int, List[Move]) = 
	{
		var alpha = alp
		var beta = bet
		nodesVisited += 1
		if (board.countRepetitions >= 2) // threefold repetition
			return (0, Nil)
		val score = board.getPlayerScore(opponent)
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
			orderMoves(moves, size, board, false, depth) 
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
					val childMove  = quiescence(board, !max, alpha, beta, depth + 1)
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
					if (board.whoseMove == opponent) (-AI.MATE + depth, Nil) 
					   	else (AI.MATE - depth, Nil)
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
			orderMoves(moves, size, board, false, 0)
			while (i < size)
			{
				if (System.currentTimeMillis > stopTime) // time control
					return (0, Nil)

				val move = moves(i)
				if (board.makeMove(move))
				{
					validMoves += 1
					val childMove  = quiescence(board, !max, alpha, beta, depth + 1)
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
					if (board.whoseMove == opponent) (-AI.MATE + depth, Nil) 
					   	else (AI.MATE - depth, Nil)
				}
				else
					(0, Nil)
		}
	}
}

object AI
{
	final val MATE = 1000000
 }

