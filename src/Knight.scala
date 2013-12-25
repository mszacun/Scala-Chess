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

	def generateMoves(b : Board) = 
	{
		var result : MutableList[Move] = new MutableList[Move]
		Knight.possibleMovesDirection.foreach((dir : Int) => 
		{
			val tmpPos = position + dir
			if (b.isOccupiedByOpponent(tmpPos, color))
				result += new CaptureMove(position, tmpPos, b.castlingRights)
			else
				if (b.isEmpty(tmpPos))
					result += new QuietMove(position, tmpPos, 0, b.castlingRights)
		})
		result
	}

	/* TODO: Implement */
	def rank : Int = 0
}


