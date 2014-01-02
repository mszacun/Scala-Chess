package src;

import scala.collection.mutable.MutableList

class Rook(pos : Int, col : Int, identifier : Int)
	extends Piece(pos, col, identifier, Piece.ROOK)
{
	def this(position : String, color : Int, id : Int)= 
		this(Cord.fromString(position), color, id)

	def generateDirectionMoves(b : Board, moveList : Array[Move], index : Int, dir : Int) : Int = 
	{
		var tmpPos = position
		var ind = index

		// calculate castlingRights after this rook move
		// index of castling in board.castilngRights array, that will be impossible
		// after move of this rook
		val castleDeniedAfterMove = position match
		{
			case 28 => 0 // H1
			case 21 => 1 // A1
			case 98 => 2 // H8
			case 91 => 3 // H1
			case _ => 4
		}

		val castlingRightsAfter = Array(b.castlingRights(0), b.castlingRights(1),
			b.castlingRights(2), b.castlingRights(3), false)
		castlingRightsAfter(castleDeniedAfterMove) = false

		while (true)
		{
			tmpPos += dir
			if (b.isEmpty(tmpPos))
			{
				moveList(ind) = new QuietMove(position, tmpPos, 0, castlingRightsAfter)
				ind += 1
			}
			else
			{
				if (b.isOccupiedByOpponent(tmpPos, color))
				{
					moveList(ind) = new CaptureMove(position, tmpPos, castlingRightsAfter)
					ind += 1
				}
				return ind
			}
		}
		// never reaches here
		ind
	}

	override def generateMoves(b : Board, moveList : Array[Move], index : Int) = 
	{
		var result = index
		Rook.possibleDirections.foreach((dir : Int) => 
			result = generateDirectionMoves(b, moveList, result, dir))

		result
	}

	/* TODO: Implement */
	override def rank : Int = 0
}

object Rook
{
	// posible move direction for rook
	val possibleDirections = Array(1, -1, 10, -10)
}
