package test

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
import src.Hash

object InteractiveTest extends App
{

	val startFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
	val board = Board(startFEN)

	var input = ""

	while (input != "q")
	{
		println(board.toFen)
		println("Hashcode: " + board.boardHash.toHexString)
		println("EnPassant: " + Cord.toString(board.enPassant))
		println("Castling rights: " + board.castlingRights.toBinaryString)
		print("> ")
		input = readLine

		if (input == "b")
			board.undoMove
		else if (input != 'q')
		{
			val start = input.substring(0, 2)
			val end = input.substring(2,4)
			val desiredMove = board.generateMovesForNextPlayer._1.filter((m : Move) =>
				m != null && Cord.fromString(start) == m.start && Cord.fromString(end) == m.end)

			board.makeMove(desiredMove.head)
		}
		board.updateBoardHash
	}
}

