package src;

import scala.collection.mutable.MutableList

class Queen(pos : Int, col : Int, identifier : Int)
	extends Piece(pos, col, identifier, Piece.QUEEN)
{
	val possibleDirections = Array(9, 11, -9, -11, 1, -1, 10, -10)

	def this(position : String, color : Int, id : Int)= 
		this(Cord.fromString(position), color, id)

	def generateDirectionMoves(b : Board, acc : MutableList[Move], dir : Int) :MutableList[Move] = 
	{
		var tmpPos = position
		var result = acc
		while (true)
		{
			tmpPos += dir
			if (b.isEmpty(tmpPos))
				result += new QuietMove(position, tmpPos, 0, 0, b.castlingRights)
			else
			{
				if (b.isOccupiedByOpponent(tmpPos, color))
					result += new CaptureMove(position, tmpPos, b.castlingRights)
				return result
			}
		}
		// never reaches here
		result
	}

	override def generateMoves(b : Board) = 
	{
		var result : MutableList[Move] = new MutableList[Move]
		possibleDirections.foreach((dir : Int) => 
			result = generateDirectionMoves(b, result, dir))

		result
	}

	/* TODO: Implement */
	override def rank : Int = 0
}
