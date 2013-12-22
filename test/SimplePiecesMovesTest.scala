package test;

import src.Pawn;

class SimplePiecesMovesTest extends Test
{
	def doAllTests = 
	{
		TestPawnMovesGeneration
	}

	def TestPawnMovesGeneration = 
	{
		val pawn = new Pawn("D2");
		val board = new Board();

		board.addPiece(pawn);

		val moves = pawn.generateQuietMoves(board)
		val attacks = pawn.generateAttacks(board)

		// assertions
		assert(moves.exists((field : Int) => (Cord.toString(field) == "D4")))
		assert(moves.exists((field : Int) => (Cord.toString(field) == "D3")))
		assert(attacks.isEmpty)
	}
}
