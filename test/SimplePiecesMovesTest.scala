package test

import src.Pawn
import src.Board
import src.Cord
import src.Move
import src.Piece;
import src.Knight;

class SimplePiecesMovesTest extends Test("SimplePiecesMovesTest")
{
	def doAllTests = 
	{
		TestPawnMovesGeneration
		TestPawnMovesGenerationWithOtherPieceOnBoard
		TestBlackPawnMovesGeneration
		TestPawnMovesWithAttacksGeneration
		TestPawnAttacksOnOwnPieces
		TestKnightMoveGeneration
	}

	def TestPawnMovesGeneration = 
	{
		val pawn = new Pawn("D2", Piece.WHITE, Board.WHITE_PAWN_1)
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
		val pawn = new Pawn("G7", Piece.BLACK, Board.BLACK_PAWN_2)
		val otherPiece = new Pawn("G6", Piece.WHITE, Board.WHITE_PAWN_1)
		val board = new Board()

		board.addPiece(pawn)
		board.addPiece(otherPiece)

		// there should be no possible moves
		assert(pawn.generateMoves(board).isEmpty)
	}

	def TestBlackPawnMovesGeneration = 
	{
		val pawn = new Pawn("A4", Piece.BLACK, Board.BLACK_PAWN_1)
		val board = new Board()

		board.addPiece(pawn)

		val moves = pawn.generateMoves(board)

		assert(moves.size == 1)
		assert(moves.exists((m : Move) => (Cord.toString(m.end) == "A3")))
	}

	def TestPawnMovesWithAttacksGeneration = 
	{
		val pawn = new Pawn("C2", Piece.WHITE, Board.WHITE_PAWN_3)
		val target1 = new Pawn("D3", Piece.BLACK, Board.BLACK_PAWN_1)
		val target2 = new Pawn("B3", Piece.BLACK, Board.BLACK_PAWN_2)
		val board = new Board()

		board.addPiece(pawn)
		board.addPiece(target1)
		board.addPiece(target2)

		val moves = pawn.generateMoves(board)

		// assertions
		// two attacks, and two moves, because we're on starting position
		assert(moves.size == 4) 

		assert(moves.exists((m : Move) => (Cord.toString(m.end) == "D3")))
		assert(moves.exists((m : Move) => (Cord.toString(m.end) == "B3")))
		assert(moves.exists((m : Move) => (Cord.toString(m.end) == "C3")))
		assert(moves.exists((m : Move) => (Cord.toString(m.end) == "C4")))
	}

	def TestPawnAttacksOnOwnPieces = 
	{
		val pawn = new Pawn("C4", Piece.BLACK, Board.BLACK_PAWN_1)
		val target1 = new Pawn("B3", Piece.BLACK, Board.BLACK_PAWN_1)
		val target2 = new Pawn("D3", Piece.BLACK, Board.BLACK_PAWN_2)
		val board = new Board()

		board.addPiece(pawn)
		board.addPiece(target1)
		board.addPiece(target2)

		// no attacks present here
		assert(pawn.generateAttacks(board).isEmpty)
	}

	def TestKnightMoveGeneration = 
	{
		val knight = new Knight("D4", Piece.BLACK, Board.BLACK_KNIGHT_1)
		val target1 = new Pawn("E6", Piece.WHITE, Board.WHITE_PAWN_1)
		val target2 = new Pawn("C2", Piece.WHITE, Board.WHITE_PAWN_2)
		val ownPiece = new Pawn("B5", Piece.BLACK, Board.BLACK_PAWN_1)

		val board = new Board()

		board.addPiece(knight)
		board.addPiece(target1)
		board.addPiece(target2)
		board.addPiece(ownPiece)

		val moves = knight.generateMoves(board)
		// 7 possible moves
		assert(moves.size == 7)

		// two atacks
		val attacks = moves.filter((m : Move) => m.moveType == Move.CAPTURE_MOVE)
		assert(attacks.size == 2)
		assert(attacks.exists((m : Move) => Cord.toString(m.end) == "E6"))
		assert(attacks.exists((m : Move) => Cord.toString(m.end) == "C2"))
		
	}

	def TestBishioMoveGeneration = 
	{
		val bishop = new Bishop("E5", Piece.WHITE, Board.WHITE_BISHOP_1)
		val target1 = new Knight("C3", Piece.BLACK, Board.BLACK_KNIGHT_1)
		val target2 = new Pawn("G7", Piece.BLACK, Board.BLACK_PAWN_1)
		val ally = new Knight("D6", Piece.WHITE, Board.WHITE_KNIGHT_1)
		val unreachableTarget = new Pawn("B8", Piece.BLACK, Board.BLACK_PAWN_2)

		val board = new Board()

		board.addPiece(bishop)
		board.addPiece(target1)
		board.addPiece(target2)
		board.addPiece(ally)
		board.addPiece(unreachableTarget)

		val moves = bishop.generateMoves(board)
		// 7 possible moves
		assert(moves.size == 2 + 2 + 3)

		// two attacks
		val attacks = moves.filter((m : Move) => m.moveType == Move.CAPTURE_MOVE)
		assert(attacks.size == 2)
		assert(attacks.exists((m : Move) => Cord.toString(m.end) == "C3"))
		assert(attacks.exists((m : Move) => Cord.toString(m.end) == "G7"))

}

