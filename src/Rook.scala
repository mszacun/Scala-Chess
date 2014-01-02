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

		// calculates bit mask that should be anded with castlingRights to
		// get castling rights after this move has been made
		val castlingRightsAfter = position match
		{
			case 28 => 14 // H1
			case 21 => 13 // A1
			case 98 => 11 // H8
			case 91 => 7 // H1
			case _ => 15 
		}

		while (true)
		{
			tmpPos += dir
			if (b.isEmpty(tmpPos))
			{
				moveList(ind) = new QuietMove(position, tmpPos, 0, castlingRightsAfter, false)
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

	def generateDirectionAttacks(b : Board, moveList : Array[Move], index : Int, dir : Int) : Int = 
	{
		var tmpPos = position
		var ind = index

		// calculates bit mask that should be anded with castlingRights to
		// get castling rights after this move has been made
		val castlingRightsAfter = position match
		{
			case 28 => 14 // H1
			case 21 => 13 // A1
			case 98 => 11 // H8
			case 91 => 7 // H1
			case _ => 15 
		}

		while (true)
		{
			tmpPos += dir
			if (!b.isEmpty(tmpPos))
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


	override def generateAttacks(b : Board, moveList : Array[Move], index : Int) = 
	{
		var result = index
		Rook.possibleDirections.foreach((dir : Int) => 
			result = generateDirectionAttacks(b, moveList, result, dir))

		result
	}

	override def generateMoves(b : Board, moveList : Array[Move], index : Int) = 
	{
		var result = index
		Rook.possibleDirections.foreach((dir : Int) => 
			result = generateDirectionMoves(b, moveList, result, dir))

		result
	}

	def rank(b : Board) = Rook.pieceValue + 
		Rook.positionValue(color)(Cord.from120to64(position))
}

object Rook
{
	// posible move direction for rook
	final val possibleDirections = Array(1, -1, 10, -10)

	final val pieceValue = 500

	final val positionValue = Array(
		Array(
			0, 0, 5, 10, 10, 5, 0, 0,
			25,	25,	25,	25,	25,	25,	25,	25,
			0, 0, 5, 10, 10, 5, 0, 0,
			0, 0, 5, 10, 10, 5, 0, 0,
			0, 0, 5, 10, 10, 5,	0, 0,
			0, 0, 5, 10, 10, 5,	0, 0,
			0, 0, 5, 10, 10, 5, 0, 0,
			0, 0, 5, 10, 10, 5,	0, 0),
		Array(
			0, 0, 5, 10, 10, 5,	0, 0,
			0, 0, 5, 10, 10, 5, 0, 0,
			0, 0, 5, 10, 10, 5,	0, 0,
			0, 0, 5, 10, 10, 5,	0, 0,
			0, 0, 5, 10, 10, 5, 0, 0,
			0, 0, 5, 10, 10, 5, 0, 0,
			25,	25,	25,	25,	25,	25,	25,	25,
			0, 0, 5, 10, 10, 5, 0, 0)
		)
}
