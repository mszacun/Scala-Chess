package src;

import scala.collection.mutable.MutableList

abstract class Piece(var position : Int, final val color : Int, final val id : Int,
	final val pieceType : Int)
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
	def generateMoves(b : Board, list:Array[Move], index : Int) : Int

	// rank value of this piece in actual position
	// takes under consideration material value of piece and
	// actual position of piece on board
	def rank(b : Board) : Int

}

object Piece
{
	final val WHITE = 1
	final val BLACK = 0

	final val PAWN = 1
	final val KNIGHT = 2
	final val BISHOP = 3
	final val ROOK = 4
	final val QUEEN = 5
	final val KING = 6
}
