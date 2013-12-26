package test;

import scala.collection.mutable.Map

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

// All values thanks to: http://chessprogramming.wikispaces.com/Perft+Results

class MoveGenerationTest extends Test("MoveGenerationTest")
{
	var leafNodes = 0
	var captures = 0
	var enPassants = 0
	var castles = 0
	var promotions = 0
	var checks = 0
	var checkMates = 0

	val max_indent = 10

	def doAllTests = 
	{
		StartingMovesGenerationTest
		DifficultPositionMoveGenerationTest
		MostDifficultPositionEverTest
	}



	def doesCausesCheckForMe(m : Move, b : Board) = 
	{
		val myColor = b.whoseMove
		val myKingID = if (myColor == Piece.WHITE) Board.WHITE_KING
		             else                        Board.BLACK_KING

		b.makeMove(m)
		val king = b.piecesList(myKingID)
		val result = b.isAttacked(king.position, myColor)
		b.undoMove
		result
	}

	def generateOnlyValidMoves(b : Board) = 
	{
		val allMoves = b.generateMovesForNextPlayer
		allMoves.filter((m : Move) => !doesCausesCheckForMe(m, b))
	}

	def print_indent(str : String, indent : Int) = 
	{
		for (i <- 1 to indent)
			print(" ")
		println(str)
	}

	def divide(board : Board, depth : Int)
	{
		board.generateMovesForNextPlayer.foreach((m : Move) =>
			{
				board.makeMove(m)
				println(m + ": " + startPerft(board.toFen, depth - 1)._1)
				board.undoMove
			})
	}


	def startPerft(fen : String, depth : Int) : (Int, Int, Int, Int, Int, Int, Int) =
	{
		if (depth == 0)
			(1, 0, 0, 0, 0, 0, 0)
		else
		{
			val board = Board(fen)

			leafNodes = 0
			captures = 0
			enPassants = 0
			castles = 0
			promotions = 0
			checks = 0
			checkMates = 0

			perft(board, depth - 1, null)
			(leafNodes, captures, enPassants, castles, promotions, checks, checkMates)
		}
	}

	def perft(board : Board, depth : Int, move : Move) : Unit = 
	{
		val whoseKing = board.whoseMove ^ 1
		val kingToCheckID = if (whoseKing == Piece.WHITE) Board.WHITE_KING
							else                          Board.BLACK_KING
		val pos = board.piecesList(kingToCheckID).position
		if (!board.isAttacked(pos, whoseKing))
		{
			if (depth == 0) 
			{
				val kingToCheckID = if (board.whoseMove == Piece.WHITE) Board.BLACK_KING
									else                                Board.WHITE_KING
				// check if we have check in this leaf node
				val moves = generateOnlyValidMoves(board)
				moves.foreach((m: Move) => 
				{
					board.makeMove(m)
					if (board.isAttacked(board.piecesList(kingToCheckID).position, board.whoseMove))
					{
						checks += 1
						if (generateOnlyValidMoves(board).size == 0)
							checkMates += 1
					}
					board.undoMove
				})
				leafNodes += moves.size
				captures += moves.count((m : Move) => 
					m.moveType == Move.CAPTURE_MOVE || m.moveType == Move.ENPASSANT_MOVE)
				enPassants += moves.count((m : Move) => m.moveType == Move.ENPASSANT_MOVE)
				val castles1 = moves.count((m : Move) => m.moveType == Move.CASTLE_MOVE)
				castles += castles1
				promotions += moves.count((m : Move) => m.moveType == Move.PROMOTION_MOVE)
			}
			else
			{
			
				board.generateMovesForNextPlayer.foreach((m : Move) =>
					{
//				print_indent("Taking move: " + Cord.toString(m.start) + " => " +Cord.toString(m.end),
//							max_indent - depth)
						board.makeMove(m)
						perft(board, depth - 1, m)
						board.undoMove
					})
			}
		}
		// return value
		()
	}

	def StartingMovesGenerationTest = 
	{
		val startFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

		println("Starting position: ")
		var start = System.nanoTime
		assert(startPerft(startFEN, 1) == (20, 0, 0, 0, 0, 0, 0))
		var end = System.nanoTime
		println("Depth 1: " + (end - start) + "ns")

		start = System.nanoTime
		assert(startPerft(startFEN, 2) == (400, 0, 0, 0, 0, 0 ,0))
		end = System.nanoTime
		println("Depth 2: " + (end - start) + "ns")

		start = System.nanoTime
		assert(startPerft(startFEN, 3) == (8902, 34, 0, 0, 0, 12, 0))
		end = System.nanoTime
		println("Depth 3: " + (end - start) + "ns")	

		start = System.nanoTime
		assert(startPerft(startFEN, 4) == (197281, 1576, 0, 0, 0, 469, 8))
		end = System.nanoTime
		println("Depth 4: " + (end - start) + "ns")

		start = System.nanoTime
		assert(startPerft(startFEN, 5) == (4865609, 82719, 258, 0, 0, 27351, 347))
		end = System.nanoTime
		println("Depth 5: " + (end - start) + "ns")
	}

	def DifficultPositionMoveGenerationTest = 
	{
		val fen = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -"

		println("Dfficult position: ")
		var start = System.nanoTime
		assert(startPerft(fen, 1) == (14, 1, 0, 0, 0, 2, 0))
		var end = System.nanoTime
		println("Depth 1: " + (end - start) + " ns")

		start = System.nanoTime
		assert(startPerft(fen, 2) == (191, 14, 0, 0, 0, 10, 0))
		end = System.nanoTime
		println("Depth 2: " + (end - start) + " ns")

		start = System.nanoTime
		assert(startPerft(fen, 3) == (2812, 209, 2, 0, 0, 267, 0))
		end = System.nanoTime
		println("Depth 3: " + (end - start) + " ns")

		start = System.nanoTime
		assert(startPerft(fen, 4) == (43238, 3348, 123, 0, 0, 1680, 17))
		end = System.nanoTime
		println("Depth 4: " + (end - start) + " ns")
	}

	def MostDifficultPositionEverTest = 
	{
		val fen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - "

		println("Most difficult position ever: ")
		var start = System.nanoTime
		assert(startPerft(fen, 1) == (48, 8, 0, 2, 0, 0, 0))
		var end = System.nanoTime
		println("Depth 1: " + (end - start) + " ns")

		start = System.nanoTime
		assert(startPerft(fen, 2) == (2039, 351, 1, 91, 0, 3, 0))
		end = System.nanoTime
		println("Depth 2: " + (end - start) + " ns")

		start = System.nanoTime
		assert(startPerft(fen, 3) == (97862, 17102, 45, 3162, 0, 993, 1))
		println(startPerft(fen, 3))
		end = System.nanoTime
		println("Depth 2: " + (end - start) + " ns") 
	}
}
