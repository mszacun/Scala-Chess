package src;

import scala.collection.mutable.MutableList

object Knight
{
	val possibleMovesDirection = Array(-19, -21, -12, -8, 19, 21, 8, 12)
}

class Knight(pos : Int, col : Int, identifire : Int)
	extends Piece(pos, col, identifire, Piece.KNIGHT)
{
	
	def this(position : String, color : Int, id : Int)= 
		this(Cord.fromString(position), color, id)

	def generateMoves(b : Board, moveList : Array[Move], index : Int) = 
	{
		var ind = index
		var i = 0

		while (i < Knight.possibleMovesDirection.size)
		{
			val tmpPos = position + Knight.possibleMovesDirection(i)
			if (b.isEmpty(tmpPos))
			{
				moveList(ind) = new QuietMove(position, tmpPos, 0, b.castlingRights)
				ind += 1
			}
			else if (b.isOccupiedByOpponent(tmpPos, color))
			{
				moveList(ind) = new CaptureMove(position, tmpPos, b.castlingRights)
				ind += 1
			}
			i += 1
		}
		ind
	}

	/* TODO: Implement */
	def rank : Int = 0
}


