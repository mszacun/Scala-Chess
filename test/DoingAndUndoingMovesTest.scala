package test;

import src.Cord
import src.Pawn
import src.Piece
import src.King
import src.CastleMove
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
		WhiteCastlingRightsChangingMoveTest
		BlackEnPasantDetectionDoingAndUndoingTest
		TwoCastlesDoingAndUndoingTest
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

	def WhiteCastlingRightsChangingMoveTest = 
	{
		val rook1 = new Rook("A1", Piece.WHITE, Board.WHITE_ROOK_1)
		val rook2 = new Rook("H1", Piece.WHITE, Board.WHITE_ROOK_2)

		val board = new Board()

		board.addPiece(rook1)
		board.addPiece(rook2)

		// at the beginning everyone can castle
		assert(board.castlingRights.count((right : Boolean) => right) == 4)

		var desiredMove = rook1.generateMoves(board).head
		board.makeMove(desiredMove)

		// now we cant castle queen side
		assert(!board.castlingRights(1))
		assert(board.castlingRights.count((right : Boolean) => right) == 3)

		desiredMove = rook2.generateMoves(board).head
		board.makeMove(desiredMove)

		assert(!board.castlingRights(0))
		assert(board.castlingRights.count((right : Boolean) => right) == 2)

		// undo
		board.undoMove
		board.undoMove

		// now we should be able to castle again
		assert(board.castlingRights.count((right : Boolean) => right) == 4)
	}

	def BlackEnPasantDetectionDoingAndUndoingTest = 
	{
		val targetWhitePawn = new Pawn("H2", Piece.WHITE, Board.WHITE_PAWN_1)
		val attackingBlackPawn = new Pawn("G4", Piece.BLACK, Board.BLACK_PAWN_1)
		val board = new Board()

		board.addPiece(targetWhitePawn)
		board.addPiece(attackingBlackPawn)

		// double move pawn
		var desiredMove = board.generateMovesForNextPlayer.filter(
			(m : Move) => Cord.toString(m.end) == "H4").head
		board.makeMove(desiredMove)

		// check if en passant was detected
		assert(board.enPassant == Cord.fromString("H3"))

		// find en passant move
		val enPassantMoves = board.generateMovesForNextPlayer.filter(
			(m : Move) => m.moveType == Move.ENPASSANT_MOVE)
		assert(enPassantMoves.size == 1)

		// do en passant capture
		board.makeMove(enPassantMoves.head)

		// assertion after move
		assert(board.piecesList(targetWhitePawn.id) == null)
		assert(board.isEmpty(Cord.fromString("H2")))
		assert(board.isEmpty(Cord.fromString("G4")))
		assert(board.isEmpty(Cord.fromString("H4"))) // captured pawn was here
		assert(board.board(Cord.fromString("H3")) == attackingBlackPawn.id)
		assert(attackingBlackPawn.position == Cord.fromString("H3"))

		// no en passant rights after capture
		assert(board.isOffBoard(board.enPassant))

		// undo en passant
		board.undoMove
		// check if en passant was restored
		assert(board.enPassant == Cord.fromString("H3"))

		assert(board.isEmpty(Cord.fromString("H3")))
		assert(board.piecesList(targetWhitePawn.id) eq targetWhitePawn)
		assert(board.board(Cord.fromString("H4")) == targetWhitePawn.id)
		assert(board.board(Cord.fromString("G4")) == attackingBlackPawn.id)

		assert(targetWhitePawn.position == Cord.fromString("H4"))
		assert(attackingBlackPawn.position == Cord.fromString("G4"))

		// restore to beginning
		board.undoMove
		assert(board.isOffBoard(board.enPassant))
	} 

	def TwoCastlesDoingAndUndoingTest =
	{
		val whiteRook = new Rook("A1", Piece.WHITE, Board.WHITE_ROOK_1)
		val whiteKing = new King("E1", Piece.WHITE, Board.WHITE_KING)
		val blackRook = new Rook("H8", Piece.BLACK, Board.BLACK_ROOK_1)
		val blackKing = new King("E8", Piece.BLACK, Board.BLACK_KING)

		val whiteRookStartPos = Cord.fromString("A1")
		val whiteKingStartPos = Cord.fromString("E1")
		val blackRookStartPos = Cord.fromString("H8")
		val blackKingStartPos = Cord.fromString("E8")

		val whiteRookEndPos = Cord.fromString("D1")
		val whiteKingEndPos = Cord.fromString("C1")
		val blackRookEndPos = Cord.fromString("F8")
		val blackKingEndPos = Cord.fromString("G8")

		val board = new Board()
		board.addPiece(whiteRook)
		board.addPiece(whiteKing)
		board.addPiece(blackRook)
		board.addPiece(blackKing)

		val castleRightsAfterWhiteCastle = Array(false, false, board.castlingRights(2),
			board.castlingRights(3), false)

		val castle1 = new CastleMove(whiteRookStartPos, whiteRookEndPos, 
			whiteKingStartPos, whiteKingEndPos, castleRightsAfterWhiteCastle)

		board.makeMove(castle1)

		// assertions
		assert(board.isEmpty(whiteRookStartPos))
		assert(board.isEmpty(whiteKingStartPos))

		assert(Board.isKing(board.board(whiteKingEndPos)))
		assert(Board.isRook(board.board(whiteRookEndPos)))

		assert(!board.castlingRights(0))
		assert(!board.castlingRights(1))

		val castleRightsAfterBlackCastle = Array(board.castlingRights(0), board.castlingRights(1),
			false, false, false)
		val castle2 = new CastleMove(blackRookStartPos, blackRookEndPos,
			blackKingStartPos, blackKingEndPos, castleRightsAfterBlackCastle)

		board.makeMove(castle2)

		// assertions
		assert(board.isEmpty(blackRookStartPos))
		assert(board.isEmpty(blackKingStartPos))

		assert(Board.isKing(board.board(blackKingEndPos)))
		assert(Board.isRook(board.board(blackRookEndPos)))

		assert(!board.castlingRights(2))
		assert(!board.castlingRights(3))

		// undo black castle
		board.undoMove
		assert(board.isEmpty(blackRookEndPos))
		assert(board.isEmpty(blackKingEndPos))
		
		assert(Board.isKing(board.board(blackKingStartPos)))
		assert(Board.isRook(board.board(blackRookStartPos)))

		// check if castling rights are restored
		assert(board.castlingRights(2))
		assert(board.castlingRights(3))

		// undo white castle
		board.undoMove
		assert(board.isEmpty(whiteKingEndPos))
		assert(board.isEmpty(whiteRookEndPos))

		assert(Board.isKing(board.board(whiteKingStartPos)))
		assert(Board.isRook(board.board(whiteRookStartPos)))

		assert(board.castlingRights(0))
		assert(board.castlingRights(1))
	}
}

