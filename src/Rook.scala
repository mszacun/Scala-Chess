package src;

import scala.collection.mutable.MutableList

class Rook(position : Int, color : Int, id : Int)
	extends Piece(position, color, id, Piece.ROOK)
{
	def this(position : String, color : Int, id : Int)= 
		this(Cord.fromString(position), color, id)

	def generateDirectionMoves(b : Board, acc : MutableList[Move], dir : Int) : MutableList[Move] = 
	{
		var tmpPos = position
		var result = acc

		// calculate castlingRights after this rook move
		// index of castling in board.castilngRights array, that will be impossible
		// after move of this rook
		val castleDeniedAfterMove = Cord.toString(position) match
		{
			case "H1" => 0
			case "A1" => 1
			case "H8" => 2
			case "A8" => 3
			case _ => 4
		}

		val castlingRightsAfter = Array(b.castlingRights(0), b.castlingRights(1),
			b.castlingRights(2), b.castlingRights(3), false)
		castlingRightsAfter(castleDeniedAfterMove) = false

		while (true)
		{
			tmpPos += dir
			if (b.isEmpty(tmpPos))
				result += new QuietMove(position, tmpPos, 0, 0, castlingRightsAfter)
			else
			{
				if (b.isOccupiedByOpponent(tmpPos, color))
					result += new CaptureMove(position, tmpPos, castlingRightsAfter)
				return result
			}
		}
		// never reaches here
		result
	}

	override def generateMoves(b : Board) = 
	{
		var result : MutableList[Move] = new MutableList[Move]()
		Rook.possibleDirections.foreach((dir : Int) => 
			result = generateDirectionMoves(b, result, dir))

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
