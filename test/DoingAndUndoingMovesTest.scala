package test;

import src.Board
import src.Cord
import src.Bishop
import src.Piece
import src.Move

class DoingAndUndoingMovesTest extends Test("DoingAndUndoingMovesTest")
{
	def doAllTests = 
	{
		JustOneQuietMoveTest
	}

	def JustOneQuietMoveTest = 
	{
		val destination = "B1"
		val start = "F5"
		val board = new Board()
		val bishop = new Bishop(start, Piece.WHITE, Board.WHITE_BISHOP_1)
		
		board.addPiece(bishop)

		val searchedMove = bishop.generateMoves(board).filter((m : Move) =>
			Cord.toString(m.end) == destination).head
		
		board.makeMove(searchedMove)

		// check if bishop really moved
		assert(Cord.toString(bishop.position) == destination)
		assert(board.board(Cord.fromString(destination)) == bishop.id)
		assert(board.isEmpty(Cord.fromString(start)))

		// undo move
		board.undoMove

		// again check if bishop moved
		assert(Cord.toString(bishop.position) == start)
		assert(board.board(Cord.fromString(start)) == bishop.id)
		assert(board.isEmpty(Cord.fromString(destination)))
	}
}

