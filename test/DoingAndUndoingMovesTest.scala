package test;

import src.Cord
import src.Pawn
import src.Piece
import src.King
import src.Rook
import src.Bishop
import src.Board
import src.Move

class DoingAndUndoingMovesTest extends Test("DoingAndUndoingMovesTest")
{
	def doAllTests = 
	{
		JustOneQuietMoveTest
		OneCaptureMoveTest
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

	def OneCaptureMoveTest = 
	{
		val start = "D8"
		val end = "D4"

		val rook = new Rook(start, Piece.BLACK, Board.BLACK_ROOK_1)
		val pawn = new Pawn(end, Piece.WHITE, Board.WHITE_PAWN_1)
		val board = new Board()

		board.addPiece(rook)
		board.addPiece(pawn)

		val desiredMove = rook.generateMoves(board).filter((m : Move) =>
			Cord.toString(m.end) == end).head

		board.makeMove(desiredMove)

		// check if pawn was captured, and rook has moved
		assert(Cord.toString(rook.position) == end)

		// captured piece position shouldnt change
		assert(Cord.toString(pawn.position) == end) 

		assert(board.board(Cord.fromString(end)) == rook.id)
		assert(board.isEmpty(Cord.fromString(start)))

		// undo move
		board.undoMove

		// check if everything is the same as before move
		assert(Cord.toString(rook.position) == start)
		assert(board.board(Cord.fromString(start)) == rook.id)

		assert(Cord.toString(pawn.position) == end)
		assert(board.board(Cord.fromString(end)) == pawn.id)
	}
}

