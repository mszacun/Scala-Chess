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

class DoingAndUndoingMovesTest extends Test("DoingAndUndoingMovesTest")
{
	/* TODO: When hashing board will be ready add testing it here! */
	def doAllTests = 
	{
		JustOneQuietMoveTest
		OneCaptureMoveTest
		FewMovesSequenceTest
		PromotionMoveTest
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
		assert(board.piecesList(pawn.id) == null)

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

	def FewMovesSequenceTest = 
	{
		val pawn = new Pawn("D4", Piece.BLACK, Board.BLACK_PAWN_1)
		val rook = new Rook("G3", Piece.BLACK, Board.BLACK_ROOK_1)
		val queen = new Queen("B5", Piece.WHITE, Board.WHITE_QUEEN)
		val board = new Board()

		board.addPiece(pawn)
		board.addPiece(rook)
		board.addPiece(queen)

		// firstly move pawn
		var desiredMove = pawn.generateMoves(board).filter((m : Move) =>
			Cord.toString(m.end) == "D3").head
		board.makeMove(desiredMove)

		// capture pawn with queen
		desiredMove = queen.generateMoves(board).filter((m : Move) =>
			m.moveType == Move.CAPTURE_MOVE).head
		board.makeMove(desiredMove)

		// capture queen with rook
		desiredMove = rook.generateMoves(board).filter((m : Move) =>
			m.moveType == Move.CAPTURE_MOVE).head
		board.makeMove(desiredMove)

		// check situation after this moves
		assert(board.isEmpty(Cord.fromString("D4")))
		assert(board.isEmpty(Cord.fromString("B5")))
		assert(board.isEmpty(Cord.fromString("G3")))
		assert(board.board(Cord.fromString("D3")) == rook.id)
		assert(board.piecesList(queen.id) == null)
		assert(board.piecesList(pawn.id) == null)

		// undo this 3 moves
		board.undoMove
		board.undoMove
		board.undoMove

		// make sure everything is the same as at the beginning
		assert(Cord.toString(pawn.position) == "D4")
		assert(Cord.toString(rook.position) == "G3")
		assert(Cord.toString(queen.position) == "B5")

		// check if in piecesList are certainly the same pieces
		assert(board.piecesList(pawn.id) eq pawn)
		assert(board.piecesList(rook.id) eq rook)
		assert(board.piecesList(queen.id) eq queen)

		// check is also in board array representation everything in ok
		assert(board.board(Cord.fromString("D4")) == pawn.id)
		assert(board.board(Cord.fromString("G3")) == rook.id)
		assert(board.board(Cord.fromString("B5")) == queen.id)
	}

	def PromotionMoveTest = 
	{
		val pawn = new Pawn("H7", Piece.WHITE, Board.WHITE_PAWN_1)
		val board = new Board()

		board.addPiece(pawn)

		val desiredMove = pawn.generateMoves(board).head

		board.makeMove(desiredMove)

		// assertions
		assert(board.isEmpty(Cord.fromString("H7")))
		assert(board.board(desiredMove.end) == pawn.id)
		assert(board.piecesList(pawn.id).pieceType == Piece.QUEEN)

		// undo
		board.undoMove

		assert(pawn.pieceType == Piece.PAWN)
		assert(board.isEmpty(desiredMove.end))
		assert(pawn.position == desiredMove.start)
		assert(board.board(pawn.position) == pawn.id)
	}
}

