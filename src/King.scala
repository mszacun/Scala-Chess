package src;

import scala.collection.mutable.MutableList

class King(pos : Int, col : Int, identifier : Int)
	extends Piece(pos, col, identifier, Piece.KING)
{

	def this(position : String, color : Int, id : Int)= 
		this(Cord.fromString(position), color, id)

	override def generateMoves(b : Board, moveList : Array[Move], index : Int) = 
	{
		var ind = index

		// if you move king, you loose all castling rights
		var castlingRightsAfter : Seq[Boolean] = null
		if (color == Piece.WHITE)
			castlingRightsAfter = Array(false, false, b.castlingRights(2),
				 b.castlingRights(3), false)
		else
			castlingRightsAfter = Array(b.castlingRights(0), b.castlingRights(1),
				false, false, false)

		King.possibleDirections.foreach((dir : Int) => 
		{
			val tmpPos = position + dir
			if (b.isEmpty(tmpPos))
			{
				moveList(ind) = new QuietMove(position, tmpPos, 0, castlingRightsAfter)
				ind += 1
			}
			else
				if (b.isOccupiedByOpponent(tmpPos, color))
				{
					moveList(ind) = new CaptureMove(position, tmpPos, castlingRightsAfter)
					ind += 1
				}
		})

		ind
	}

	/* TODO: Implement */
	override def rank : Int = 0
}

object King
{
	val possibleDirections = Array(9, 11, -9, -11, 1, -1, 10, -10)
}
