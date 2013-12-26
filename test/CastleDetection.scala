package test

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

class CastleDetectionTest extends Test("CastleDetectionTest")
{
	def doAllTests = 
	{
		AllCastlesPossibleTest
	}

	def AllCastlesPossibleTest =
	{
		val fen = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1"
		val board = Board(fen)

		var whiteMoves = board.generateMovesForNextPlayer
		var whiteCastles = whiteMoves.filter((m : Move) => m.moveType == Move.CASTLE_MOVE)
		assert(whiteCastles.size == 2)
		board.makeMove(whiteCastles.head)

		var blackMoves = board.generateMovesForNextPlayer
		var blackCastles = blackMoves.filter((m : Move) => m.moveType == Move.CASTLE_MOVE)
		// only one side, becasue white rook attack square between black rook and king
		assert(blackCastles.size == 1) 
		board.makeMove(blackCastles.head)

		whiteMoves = board.generateMovesForNextPlayer
		whiteCastles = whiteMoves.filter((m : Move) => m.moveType == Move.CASTLE_MOVE)
		assert(whiteCastles.size == 0) // now no castling rights
	}

	def CorrectCastlePositionButNoRightsTest = 
	{
		val fen = "r3k2r/8/1n6/8/6N1/2B5/PPPPPPPP/R3K2R b - - 0 1"
		val board = Board(fen)

		var moves = board.generateMovesForNextPlayer

		assert(moves.count((m : Move) => m.moveType == Move.CASTLE_MOVE) == 0)
	}

}

