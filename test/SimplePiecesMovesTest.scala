package test

import src.Pawn
import src.Board
import src.Cord
import src.Move

class SimplePiecesMovesTest extends Test
{
	def doAllTests = 
	{
		TestPawnMovesGeneration
	}

	def TestPawnMovesGeneration = 
	{
		val pawn = new Pawn("D2", true, Board.WHITE_PAWN_1);
		val board = new Board();

		board.addPiece(pawn);

		val moves = pawn.generateQuietMoves(board)
		val attacks = pawn.generateAttacks(board)

		// assertions
		attacks.foreach((m : Move) => println(Cord.toString(m.end)))
		assert(moves.exists((m : Move) => (Cord.toString(m.end) == "D4")))
		assert(moves.exists((m : Move) => (Cord.toString(m.end) == "D3")))
		assert(attacks.isEmpty)
	}
}
