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

class MoveGenerationTest extends Test("MoveGenerationTest")
{
	def StartingMovesGenerationTest = 
	{
		val startFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
		val board = Board(startFEN)

		val moves = board.generateMoves(board.whoseMove)

		assert(move.size == 20)
	}
}
