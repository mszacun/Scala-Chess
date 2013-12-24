package src;

import scala.collection.mutable.MutableList

abstract class Piece(var position : Int, val color : Int, val id : Int,
	val pieceType : Int)
{
	// * position -> actual position on board as index in array
	// representing board
	// * color -> 1 - white, 0 - black
	// * id -> id of this particular piece in board piece list, see Board class

	// constructs piece from human-readable field name
	def this(position : String, color : Int, id : Int, pieceType : Int) = 
		this(Cord.fromString(position), color, id, pieceType)

	// WARNING: All generating moves methods may return moves which destination is
	// outside the board! Board class is responsible for checking this, also
	// generated moves may case own king to be in check, Board class also has to
	// take care of it

	// quiteMoves + attacks
	def generateMoves(b : Board) : MutableList[Move]

	// rank value of this piece in actual position
	// takes under consideration material value of piece and
	// actual position of piece on board
	def rank : Int
}

object Piece
{
	val WHITE = 1
	val BLACK = 0

	val PAWN = 1
	val KNIGHT = 2
	val BISHOP = 3
	val ROOK = 4
	val QUEEN = 5
	val KING = 6
}
