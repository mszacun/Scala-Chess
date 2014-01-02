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
import src.AI

object MoveSearchingTest extends App
{
	//val startFEN = "r1b1k2r/ppppnppp/2n2q2/2b5/3NP3/2P1B3/PP3PPP/RN1QKB1R w KQkq - 0 1"
	val startFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
	val board = Board(startFEN)
	val ai = new AI

	val (score, move) = ai.findNextMove(board, board.whoseMove ^ 1, 6)
	println("Move: " + move + ", Score: " + score)
	println("Nodes: " + ai.allNodesVisited)
}
