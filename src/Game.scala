package src

object Game extends App
{
	val startFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
	val board = Board(startFEN)
	var flag = false
	var desiredMove : Array[Move] = null
	var opp = Piece.BLACK
	val thinkingTime = 5 * 1000
	
	val if_we_start = readLine
	if (if_we_start == "t")
	{
	  	opp = Piece.WHITE
	  	while (!flag)
		{
			print("Start: ")
			val a = readLine
			println("End: ")
			val b = readLine
		
			desiredMove  = board.generateMovesForNextPlayer._1.filter(
					(m : Move) => m != null && Cord.toString(m.start) == a && Cord.toString(m.end) == b)
			flag = desiredMove.size > 0
		}
	  board.makeMove(desiredMove.head)
	}
	
	val ai = new AI(opp)
	while (true)
	{
	    println; println
		val start = System.currentTimeMillis()
		val (score, move) = ai.findNextMove(board, thinkingTime)
		val end = System.currentTimeMillis()
		
		println("" + move.head + " score: " + score)
		println("Time: " + (end - start) + " ms")
		println("Nodes visited: " + ai.allNodesVisited + " depth: " + move.size)
		println
		board.makeMove(move.head)
		println("Board: " + board.toFen)
		
		flag = false
		
		val (moves, size) = board.generateMovesForNextPlayer
		while (!flag)
		{
			print("Start: ")
			val a = readLine
			println("End: ")
			val b = readLine
			if (a == "back")
			{
				board.undoMove
				board.undoMove
			}
		
			desiredMove  = board.generateMovesForNextPlayer._1.filter(
					(m : Move) => m != null && Cord.toString(m.start) == a && Cord.toString(m.end) == b)
			flag = desiredMove.size > 0
		}
		    
	    board.makeMove(desiredMove.head) 
	}
}
 