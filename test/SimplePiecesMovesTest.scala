package test

import src.Pawn
import src.Board
import src.Cord
import src.Move
import src.Piece
import src.Knight
import src.Bishop
import src.Rook
import src.Queen
import src.King

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
		TestBishioMoveGeneration
		TestRookMoveGeneration
		TestQueenMoveGeneration
		TestKingMoveGeneration
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

	def TestRookMoveGeneration = 
	{
		val rook = new Rook("E7", Piece.BLACK, Board.BLACK_ROOK_1)
		val target1 = new Pawn("E3", Piece.WHITE, Board.WHITE_PAWN_1)
		val target2 = new Bishop("G7", Piece.WHITE, Board.WHITE_BISHOP_1)
		val unreachableTarget = new Knight("E1", Piece.WHITE, Board.WHITE_KNIGHT_1)
		val ally = new Rook("C7", Piece.BLACK, Board.BLACK_ROOK_2)

		val board = new Board()

		board.addPiece(rook)
		board.addPiece(target1)
		board.addPiece(target2)
		board.addPiece(unreachableTarget)
		board.addPiece(ally)

		val moves = rook.generateMoves(board)

		assert(moves.size == 2 + 1 + 4 + 1)
		val attacks = moves.filter((m : Move) => m.moveType == Move.CAPTURE_MOVE)
		assert(attacks.size == 2)
		assert(attacks.exists((m : Move) => Cord.toString(m.end) == "E3"))
		assert(attacks.exists((m : Move) => Cord.toString(m.end) == "G7"))

	}

	def TestQueenMoveGeneration = 
	{
		val queen = new Queen("D4", Piece.WHITE, Board.WHITE_QUEEN)
		val target1 = new Pawn("H8", Piece.BLACK, Board.BLACK_PAWN_1) // 4 moves
		val target2 = new Knight("D6", Piece.BLACK, Board.BLACK_KNIGHT_1) // 2 moves
		val target3 = new Bishop("A4", Piece.BLACK, Board.BLACK_BISHOP_1) // 3 moves
		val ally1 = new Rook("D2", Piece.WHITE, Board.WHITE_ROOK_1) // 1 move
		val ally2 = new Pawn("G4", Piece.WHITE, Board.WHITE_PAWN_1) // 2 moves

		// 3 moves NE possible
		// 3 moves NW possible
		// 3 move SE possible
		val board = new Board()

		board.addPiece(queen)
		board.addPiece(target1)
		board.addPiece(target2)
		board.addPiece(target3)
		board.addPiece(ally1)
		board.addPiece(ally2)

		val moves = queen.generateMoves(board)

		assert(moves.size == 4 + 2 + 3 + 1 +2 + 3 + 3 + 3)

		val attacks = moves.filter((m : Move) => m.moveType == Move.CAPTURE_MOVE)
		assert(attacks.size == 3)
		assert(attacks.exists((m : Move) => Cord.toString(m.end) == "H8"))
		assert(attacks.exists((m : Move) => Cord.toString(m.end) == "D6"))
		assert(attacks.exists((m : Move) => Cord.toString(m.end) == "A4"))
	}

	def TestKingMoveGeneration = 
	{
		// WARNING: king's generateMoves returns also move, which causes check!

		val king = new King("A5", Piece.BLACK, Board.BLACK_KING)
		val target = new Bishop("A4", Piece.WHITE, Board.WHITE_BISHOP_1)
		val attacker = new Pawn("B3", Piece.WHITE, Board.WHITE_PAWN_1)
		val ally = new Pawn("A6", Piece.BLACK, Board.BLACK_PAWN_1)

		val board = new Board()

		board.addPiece(king)
		board.addPiece(target)
		board.addPiece(attacker)
		board.addPiece(ally)

		val moves = king.generateMoves(board)

		assert(moves.size == 4)
		val attacks = moves.filter((m : Move) => m.moveType == Move.CAPTURE_MOVE)

		assert(attacks.size == 1)
		assert(attacks.exists((m : Move) => Cord.toString(m.end) == "A4"))
	}

}

