package src;

import scala.collection.mutable.MutableList;

class Bishop(pos : Int, col : Int, identifier : Int)
	extends Piece(pos, col, identifier, Piece.BISHOP)
{

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
				moveList(ind) = new QuietMove(position, tmpPos, 0, b.castlingRights)
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

	override def generateAttacks(b : Board, moveList : Array[Move], index : Int) =
	{
		var result = index
		Bishop.possibleDirections.foreach((dir : Int) => 
			result = generateDirectionAttacks(b, moveList, result, dir))

		result
	}

	override def generateMoves(b : Board, moveList : Array[Move], index : Int) = 
	{
		var result = index
		Bishop.possibleDirections.foreach((dir : Int) => 
			result = generateDirectionMoves(b, moveList, result, dir))

		result
	}

	def rank(b : Board) = Bishop.pieceValue + 
		Bishop.positionValue(color)(Cord.from120to64(position))
}

object Bishop
{
	final val possibleDirections = Array(9, 11, -9, -11)

	final val pieceValue = 325

	final val positionValue = Array(
		Array(
			-20,-10,-10,-10,-10,-10,-10,-20,
			-10,  0,  0,  0,  0,  0,  0,-10,
			-10,  0,  5, 10, 10,  5,  0,-10,
			-10,  5,  5, 10, 10,  5,  5,-10,
			-10,  0, 10, 10, 10, 10,  0,-10,
			-10, 10, 10, 10, 10, 10, 10,-10,
			-10,  5,  0,  0,  0,  0,  5,-10,
			-20,-10,-40,-10,-10,-40,-10,-20),
		Array(
			-20,-10,-40,-10,-10,-40,-10,-20,
			-10,  5,  0,  0,  0,  0,  5,-10,
			-10, 10, 10, 10, 10, 10, 10,-10,
			-10,  0, 10, 10, 10, 10,  0,-10,
			-10,  5,  5, 10, 10,  5,  5,-10,
			-10,  0,  5, 10, 10,  5,  0,-10,
			-10,  0,  0,  0,  0,  0,  0,-10,
			-20,-10,-10,-10,-10,-10,-10,-20)
		)
}
