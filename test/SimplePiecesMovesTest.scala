package test

import src.Pawn
import src.Board
import src.Cord
import src.Move

class SimplePiecesMovesTest extends Test("SimplePiecesMovesTest")
{
	def doAllTests = 
	{
		TestPawnMovesGeneration
		TestPawnMovesGenerationWithOtherPieceOnBoard
		TestBlackPawnMovesGeneration
	}

	def TestPawnMovesGeneration = 
	{
		val pawn = new Pawn("D2", true, Board.WHITE_PAWN_1)
		val board = new Board()

		board.addPiece(pawn)

		val moves = pawn.generateQuietMoves(board)
		val attacks = pawn.generateAttacks(board)

		// assertions
		attacks.foreach((m : Move) => println(Cord.toString(m.end)))
		assert(moves.exists((m : Move) => (Cord.toString(m.end) == "D4")))
		assert(moves.exists((m : Move) => (Cord.toString(m.end) == "D3")))
		assert(attacks.isEmpty)
	}

	def TestPawnMovesGenerationWithOtherPieceOnBoard = 
	{
		val pawn = new Pawn("G7", false, Board.BLACK_PAWN_2)
		val otherPiece = new Pawn("G6", true, Board.WHITE_PAWN_1)
		val board = new Board()

		board.addPiece(pawn)
		board.addPiece(otherPiece)

		// there should be no possible moves
		assert(pawn.generateMoves(board).isEmpty)
	}

	def TestBlackPawnMovesGeneration = 
	{
		val pawn = new Pawn("A4", false, Board.BLACK_PAWN_1)
		val board = new Board()

		board.addPiece(pawn)

		val moves = pawn.generateMoves(board)

		assert(moves.size == 1)
		assert(moves.exists((m : Move) => (Cord.toString(m.end) == "A3")))
	}
}
