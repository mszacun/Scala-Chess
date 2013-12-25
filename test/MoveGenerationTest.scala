package test;


import src.Cord
import src.Pawn
import src.Piece
import src.King
import src.Rook
import src.Bishop
import src.Board
import src.Queen
import src.PromotionMove
import src.Move

// All values thanks to: http://chessprogramming.wikispaces.com/Perft+Results

class MoveGenerationTest extends Test("MoveGenerationTest")
{
	var leafNodes = 0
	var captures = 0
	var checks = 0

	def doAllTests = 
	{
		StartingMovesGenerationTest
	}

	def startPerft(fen : String, depth : Int) : Int =
	{
		val board = Board(fen)

		leafNodes = 0
		captures = 0
		checks = 0

		perft(board, depth - 1)
		leafNodes
	}

	def perft(board : Board, depth : Int) : Unit = 
	{
		if (depth == 0) 
		{
			val kingToCheckID = if (board.whoseMove == Piece.WHITE) Board.WHITE_KING
								else                                Board.BLACK_KING
			// check if we have check in this leaf node
			if (board.isAttacked(board.piecesList(kingToCheckID).position, board.whoseMove))
				checks += 1
			val moves = board.generateMovesForNextPlayer
			leafNodes += moves.size
			captures += moves.count((m : Move) => m.moveType == Move.CAPTURE_MOVE)

		}
		else
		{
			// if there side that make previous move is in check, then that was illegal move
			val kingToCheckID = if (board.whoseMove == Piece.WHITE) Board.BLACK_KING
								else                                Board.WHITE_KING
			if (!board.isAttacked(board.piecesList(kingToCheckID).position, board.whoseMove ^ 1))
			{
				board.generateMovesForNextPlayer.foreach((m : Move) =>
					{
						board.makeMove(m)
						perft(board, depth - 1)
						board.undoMove
					})
			}
			else println("Check position")
		}
		// return value
		()
	}

	def StartingMovesGenerationTest = 
	{
		val startFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

		assert(startPerft(startFEN, 1) == 20)
		assert(startPerft(startFEN, 2) == 400)
		assert(startPerft(startFEN, 3) == 8902)
		println(startPerft(startFEN, 4))
		println("Captures: " + captures)
		println("Checks: " + checks)
	}
}
