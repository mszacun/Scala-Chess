package test;

import src.Cord
import src.Pawn
import src.Knight
import src.Piece
import src.King
import src.Rook
import src.Bishop
import src.Board
import src.Queen
import src.Move

class CheckDetectionTest extends Test("CheckDetectionTest")
{
	def doAllTests =
	{
		TestWhitePawnChecking
		TestBlackPawnChecking
		TestSimpleNoCheck
		TestComplicatedNoCheck
		TestKnightChecking
		TestBishopChecking
		TestRookChecking
		TestQueenChecking
	}

	def TestWhitePawnChecking = 
	{
		val pawn = new Pawn("H2", Piece.WHITE, Board.WHITE_PAWN_1)
		val targetKing = new King("G3", Piece.BLACK, Board.BLACK_KING)
		val board = new Board()

		board.addPiece(pawn)
		board.addPiece(targetKing)

		assert(board.isCheck(Piece.BLACK))
	}

	def TestBlackPawnChecking = 
	{
		val pawn = new Pawn("E3", Piece.BLACK, Board.BLACK_PAWN_1)
		val targetKing = new King("F2", Piece.WHITE, Board.WHITE_KING)
		val board = new Board()

		board.addPiece(pawn)
		board.addPiece(targetKing)

		assert(board.isCheck(Piece.WHITE))
	}

	def TestSimpleNoCheck = 
	{
		val king = new King("E5", Piece.WHITE, Board.WHITE_KING)
		val board = new Board()

		board.addPiece(king)

		assert(!board.isCheck(Piece.WHITE))
	}

	def TestComplicatedNoCheck = 
	{
		val king = new King("D4", Piece.WHITE, Board.WHITE_KING)
		val enemyRook = new Rook("B2", Piece.BLACK, Board.BLACK_ROOK_1)
		val enemyBishop = new Bishop("B4", Piece.BLACK, Board.BLACK_BISHOP_1)
		val enemyPawn = new Pawn("D6", Piece.BLACK, Board.BLACK_PAWN_1)
		val allyQueen = new Queen("E3", Piece.WHITE, Board.WHITE_QUEEN)
		val allyKnight = new Knight("F3", Piece.WHITE, Board.WHITE_KNIGHT_1)
		val enemyKnight = new Knight("G4", Piece.BLACK, Board.BLACK_KNIGHT_1)

		val board = new Board()

		board.addPiece(king)
		board.addPiece(enemyRook)
		board.addPiece(enemyBishop)
		board.addPiece(enemyPawn)
		board.addPiece(allyQueen)
		board.addPiece(allyKnight)
		board.addPiece(enemyKnight)

		val start = System.currentTimeMillis;
		assert(!board.isCheck(king.color))
		println("Looking for check: " + (System.currentTimeMillis - start))
	}

	def TestKnightChecking = 
	{
		val targetKing = new King("G6", Piece.BLACK, Board.BLACK_KING)
		val knight = new Knight("H8", Piece.WHITE, Board.WHITE_KNIGHT_1)
		val board = new Board()

		board.addPiece(targetKing)
		board.addPiece(knight)

		assert(board.isCheck(targetKing.color))
	}

	def TestBishopChecking = 
	{
		val targetKing = new King("E6", Piece.BLACK, Board.BLACK_KING)
		val enemyBishop = new Bishop("C4", Piece.WHITE, Board.WHITE_BISHOP_1)
		val allyBishop = new Bishop("F7", Piece.BLACK, Board.BLACK_BISHOP_1)
		val board = new Board()

		board.addPiece(targetKing)
		board.addPiece(enemyBishop)
		board.addPiece(allyBishop)

		assert(board.isCheck(targetKing.color))
	}


	def TestRookChecking = 
	{
		val targetKing = new King("C5", Piece.WHITE, Board.WHITE_KING)
		val enemyRook = new Rook("C8", Piece.BLACK, Board.BLACK_ROOK_1)
		val allyRook = new Rook("C1", Piece.WHITE, Board.WHITE_ROOK_1)
		val board = new Board()

		board.addPiece(targetKing)
		board.addPiece(enemyRook)
		board.addPiece(allyRook)

		assert(board.isCheck(targetKing.color))
	}

	def TestQueenChecking = 
	{
		val targetKing = new King("H5", Piece.BLACK, Board.BLACK_KING)
		val enemyQueen = new Queen("E8", Piece.WHITE, Board.WHITE_QUEEN)
		val board = new Board()

		board.addPiece(targetKing)
		board.addPiece(enemyQueen)

		assert(board.isCheck(targetKing.color))
	}
}
