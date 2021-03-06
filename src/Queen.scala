package src;

import scala.collection.mutable.MutableList

class Queen(pos : Int, col : Int, identifier : Int)
	extends Piece(pos, col, identifier, Piece.QUEEN)
{
	final val possibleDirections = Array(9, 11, -9, -11, 1, -1, 10, -10)

	def this(position : String, color : Int, id : Int)= 
		this(Cord.fromString(position), color, id)

	def generateDirectionMoves(b : Board, moveList : Array[Move], index : Int, dir : Int) : Int = 
	{
		var tmpPos = position
		var ind = index

		while (true)
		{
			tmpPos += dir
			if (b.isEmpty(tmpPos))
			{
				moveList(ind) = new QuietMove(position, tmpPos, 0, b.castlingRights, false)
				ind += 1
			}
			else
			{
				if (b.isOccupiedByOpponent(tmpPos, color))
				{
					moveList(ind) = new CaptureMove(position, tmpPos, b.castlingRights)
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

		while (true)
		{
			tmpPos += dir
			if (!b.isEmpty(tmpPos))
			{
				if (b.isOccupiedByOpponent(tmpPos, color))
				{
					moveList(ind) = new CaptureMove(position, tmpPos, b.castlingRights)
					ind += 1
				}
				return ind
			}
		}
		// never reaches here
		ind
	}


	override def generateMoves(b : Board, moveList:Array[Move], index : Int) = 
	{
		var result = index
		possibleDirections.foreach((dir : Int) => 
			result = generateDirectionMoves(b, moveList, result, dir))

		result
	}

	override def generateAttacks(b : Board, moveList:Array[Move], index : Int) = 
	{
		var result = index
		possibleDirections.foreach((dir : Int) => 
			result = generateDirectionAttacks(b, moveList, result, dir))

		result
	}

	// position value tables and piece values taken from
	// http://chessprogramming.wikispaces.com/Simplified+evaluation+function
	override def rank(b : Board)  = 975
}
