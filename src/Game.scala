package src

object Game extends App
{
	val startFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
	val board = Board(startFEN)
	var flag = false
	var desiredMove : Array[Move] = null
	var opp = Piece.BLACK
	val thinkingTime = 5 * 1000
	var input = ""
	
	val if_we_start = readLine
	if (if_we_start == "t")
	{
	  	opp = Piece.WHITE
	  	while (!flag)
		{
			print("Move: ")
			input = readLine
			val start = input.substring(0, 2)
			val end = input.substring(2,4)
			desiredMove = board.generateMovesForNextPlayer._1.filter((m : Move) =>
				m != null && Cord.fromString(start) == m.start && Cord.fromString(end) == m.end)
		
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
		
		println(move)
		println("" + move.head + " score: " + score)
		println("Time: " + (end - start) + " ms")
		println("Nodes visited: " + ai.allNodesVisited + " depth: " + move.size)
		println
		board.makeMove(move.head)
		println("Board: " + board.toFen)
		board.piecesList.foreach(p => 
		{
			if (p != null && p.pieceType == Piece.PAWN)
			{
				val pawn = p.asInstanceOf[Pawn]
				if (pawn.isPassedPawn(board))
					println("Pawn on " + Cord.toString(p.position) + "is passed")
			}
		})
		
		flag = false
		
		val (moves, size) = board.generateMovesForNextPlayer
		while (!flag)
		{
			print("Move: ")
			input = readLine
			val start = input.substring(0, 2)
			val end = input.substring(2,4)
			desiredMove = board.generateMovesForNextPlayer._1.filter((m : Move) =>
				m != null && Cord.fromString(start) == m.start && Cord.fromString(end) == m.end)
			if (input == "back")
			{
				board.undoMove
				board.undoMove
			}
		
			flag = desiredMove.size > 0
		}
		    
	    board.makeMove(desiredMove.head) 
	}
}
 
