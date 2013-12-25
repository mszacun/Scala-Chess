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

class FenParsingTest extends Test("FenParsingTest")
{
	def doAllTests = 
	{
	//	StartingPositionFenParseTest
		FenWithEmptySquaresBetweenPiecesTest
	}

	def StartingPositionFenParseTest =
	{
		val fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

		val board = Board(fen)

		// test if all pieces are present on their starting position
		assert(board.board(Cord.fromString("A1")) == Board.WHITE_ROOK_1)
		assert(board.board(Cord.fromString("B1")) == Board.WHITE_KNIGHT_1)
		assert(board.board(Cord.fromString("C1")) == Board.WHITE_BISHOP_1)
		assert(board.board(Cord.fromString("D1")) == Board.WHITE_QUEEN) // make sure it's right
		assert(board.board(Cord.fromString("E1")) == Board.WHITE_KING)
		assert(board.board(Cord.fromString("F1")) == Board.WHITE_BISHOP_2)
		assert(board.board(Cord.fromString("G1")) == Board.WHITE_KNIGHT_2)
		assert(board.board(Cord.fromString("H1")) == Board.WHITE_ROOK_2)

		assert(board.board(Cord.fromString("A2")) == Board.WHITE_PAWN_1)
		assert(board.board(Cord.fromString("B2")) == Board.WHITE_PAWN_2)
		assert(board.board(Cord.fromString("C2")) == Board.WHITE_PAWN_3)
		assert(board.board(Cord.fromString("D2")) == Board.WHITE_PAWN_4)
		assert(board.board(Cord.fromString("E2")) == Board.WHITE_PAWN_5)
		assert(board.board(Cord.fromString("F2")) == Board.WHITE_PAWN_6)
		assert(board.board(Cord.fromString("G2")) == Board.WHITE_PAWN_7)
		assert(board.board(Cord.fromString("H2")) == Board.WHITE_PAWN_8)

		assert(board.board(Cord.fromString("A8")) == Board.BLACK_ROOK_1)
		assert(board.board(Cord.fromString("B8")) == Board.BLACK_KNIGHT_1)
		assert(board.board(Cord.fromString("C8")) == Board.BLACK_BISHOP_1)
		assert(board.board(Cord.fromString("D8")) == Board.BLACK_QUEEN)
		assert(board.board(Cord.fromString("E8")) == Board.BLACK_KING)
		assert(board.board(Cord.fromString("F8")) == Board.BLACK_BISHOP_2)
		assert(board.board(Cord.fromString("G8")) == Board.BLACK_KNIGHT_2)
		assert(board.board(Cord.fromString("H8")) == Board.BLACK_ROOK_2)

		assert(board.board(Cord.fromString("A7")) == Board.BLACK_PAWN_1)
		assert(board.board(Cord.fromString("B7")) == Board.BLACK_PAWN_2)
		assert(board.board(Cord.fromString("C7")) == Board.BLACK_PAWN_3)
		assert(board.board(Cord.fromString("D7")) == Board.BLACK_PAWN_4)
		assert(board.board(Cord.fromString("E7")) == Board.BLACK_PAWN_5)
		assert(board.board(Cord.fromString("F7")) == Board.BLACK_PAWN_6)
		assert(board.board(Cord.fromString("G7")) == Board.BLACK_PAWN_7)
		assert(board.board(Cord.fromString("H7")) == Board.BLACK_PAWN_8)

		// castle rights
		assert(board.castlingRights.count((right : Boolean) => right) == 4)

		// en passant
		assert(board.isOffBoard(board.enPassant1))
		assert(board.isOffBoard(board.enPassant2))

		assert(board.whoseMove == Piece.WHITE)

		// check also in opposite direction
		val expectedFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq"
		assert(board.toFen == expectedFen)
	}

	def FenWithEmptySquaresBetweenPiecesTest = 
	{
		val fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
		val board = Board(fen)

		// test if all pieces are present on their starting position
		assert(board.board(Cord.fromString("A1")) == Board.WHITE_ROOK_1)
		assert(board.board(Cord.fromString("B1")) == Board.WHITE_KNIGHT_1)
		assert(board.board(Cord.fromString("C1")) == Board.WHITE_BISHOP_1)
		assert(board.board(Cord.fromString("D1")) == Board.WHITE_QUEEN) // make sure it's right
		assert(board.board(Cord.fromString("E1")) == Board.WHITE_KING)
		assert(board.board(Cord.fromString("F1")) == Board.WHITE_BISHOP_2)
		assert(board.board(Cord.fromString("G1")) == Board.WHITE_KNIGHT_2)
		assert(board.board(Cord.fromString("H1")) == Board.WHITE_ROOK_2)

		/* TODO: Write functions for each kind of piece
		 * that will be albe to answer by given piece key, is that this kind of piece 
		 */
		assert(Board.isPawn(board.board(Cord.fromString("A2"))))
		assert(Board.isPawn(board.board(Cord.fromString("B2"))))
		assert(Board.isPawn(board.board(Cord.fromString("C2"))))
		assert(Board.isPawn(board.board(Cord.fromString("D2"))))
		assert(Board.isPawn(board.board(Cord.fromString("E4"))))
		assert(Board.isPawn(board.board(Cord.fromString("F2"))))
		assert(Board.isPawn(board.board(Cord.fromString("G2"))))
		assert(Board.isPawn(board.board(Cord.fromString("H2"))))

		assert(board.board(Cord.fromString("A8")) == Board.BLACK_ROOK_1)
		assert(board.board(Cord.fromString("B8")) == Board.BLACK_KNIGHT_1)
		assert(board.board(Cord.fromString("C8")) == Board.BLACK_BISHOP_1)
		assert(board.board(Cord.fromString("D8")) == Board.BLACK_QUEEN)
		assert(board.board(Cord.fromString("E8")) == Board.BLACK_KING)
		assert(board.board(Cord.fromString("F8")) == Board.BLACK_BISHOP_2)
		assert(board.board(Cord.fromString("G8")) == Board.BLACK_KNIGHT_2)
		assert(board.board(Cord.fromString("H8")) == Board.BLACK_ROOK_2)

		assert(board.board(Cord.fromString("A7")) == Board.BLACK_PAWN_1)
		assert(board.board(Cord.fromString("B7")) == Board.BLACK_PAWN_2)
		assert(board.board(Cord.fromString("C7")) == Board.BLACK_PAWN_3)
		assert(board.board(Cord.fromString("D7")) == Board.BLACK_PAWN_4)
		assert(board.board(Cord.fromString("E7")) == Board.BLACK_PAWN_5)
		assert(board.board(Cord.fromString("F7")) == Board.BLACK_PAWN_6)
		assert(board.board(Cord.fromString("G7")) == Board.BLACK_PAWN_7)
		assert(board.board(Cord.fromString("H7")) == Board.BLACK_PAWN_8)

		// castle rights
		assert(board.castlingRights.count((right : Boolean) => right) == 4)

		// en passant
		assert(board.isOffBoard(board.enPassant1))
		assert(board.isOffBoard(board.enPassant2))

		// whose next move
		assert(board.whoseMove == Piece.BLACK)

		val expectedFen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq"
		assert(board.toFen == expectedFen)
	}
}
