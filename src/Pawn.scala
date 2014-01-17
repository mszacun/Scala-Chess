package src;

import scala.collection.mutable.MutableList


class Pawn(pos : Int, col : Int, identifier : Int) 
	extends Piece(pos, col, identifier, Piece.PAWN)
{
	val whitePawnsStartingRow = 1 // remeber, that rows are 0 indexed
	val blackPawnsStartingRow = 6

	def this(position : String, color : Int, id : Int)= 
		this(Cord.fromString(position), color, id)

	// see in Piece
	def generateQuietMoves(b : Board, moveList : Array[Move], ind : Int) : Int =
	{
		var index = ind
		// default move
		var move = position + Pawn.moveDirection(color)
		if (b.isEmpty(move) && !b.isOffBoard(move))
		{
			if (Cord.getRow(move) == Pawn.promotionRank(color))
				Pawn.possiblePromotions.foreach((pieceType : Int) =>
				{
					moveList(index) = new PromotionMove(position, move,
						b.castlingRights, pieceType)
					index += 1
				})
			else
			{
				moveList(index) = new QuietMove(position, move, 0, 
					b.castlingRights, true)
				index += 1

				// double move if on starting position
				if (Cord.getRow(position) == Pawn.startingRow(color))
				{
					move = position + 2 * Pawn.moveDirection(color)
					if (b.isEmpty(move) && !b.isOffBoard(move))
					{
						moveList(index) = new QuietMove(position, 
							move, position + Pawn.moveDirection(color), // enPasant is possible
							b.castlingRights, true)
						index += 1
					}
				}
			}
		}
		return index
	}

	def generateAttacks(b : Board, moveList : Array[Move], ind : Int) :Int =
	{
		var index = ind
		Pawn.attackDirections(color).foreach((dir : Int) =>
		{
			val pos = position + dir
			if (b.isOccupiedByOpponent(pos, color))
				if (Cord.getRow(pos) == Pawn.promotionRank(color))
					Pawn.possiblePromotions.foreach((pieceType : Int) =>
					{
						moveList(index) = new CapturePromotionMove(position, pos,
							b.castlingRights, pieceType)
						index += 1
					})
				else
				{
					moveList(index) = new CaptureMove(position, pos, b.castlingRights)
					index += 1
				}
		})
		return index
	}

	override def generateMoves(b : Board, moveList:Array[Move], index : Int) : Int =
	{
		val ind = generateAttacks(b, moveList, index)
		generateQuietMoves(b, moveList, ind)
	}

	def rank(b : Board) : Int = 
	{
		if (!isPassedPawn(b))	
			return Pawn.pieceValue + 
				Pawn.positionValue(color)(Cord.from120to64(position))
		else
			return Pawn.pieceValue + 
				Pawn.positionValue(color)(Cord.from120to64(position)) + 
				Pawn.passedPawnBonus * math.abs(Cord.getRow(position) - Pawn.promotionRank(color))
	}


	def isPassedPawn(b : Board) : Boolean = 
	{
		var pos = position + Pawn.moveDirection(color)

		while (!b.isOffBoard(pos))
		{
			// if the is a pawn in fron, no matter what color, this pawn is not passed
			if (Board.isPawn(b.board(pos)))
				return false

			if (Board.isPawn(b.board(pos + 1)) && (b.board(pos + 1) & 1) != color)
				return false

			if (Board.isPawn(b.board(pos - 1)) && (b.board(pos - 1) & 1) != color)
				return false

			pos += Pawn.moveDirection(color)
		}

		// if we got to end of board, this pawn is passed
		return true
	}

}

object Pawn
{
	final val possiblePromotions = Array(Piece.QUEEN, Piece.BISHOP, Piece.KNIGHT,
		Piece.ROOK)

	final val pieceValue = 100

	final val passedPawnBonus = 50 / 7 // closer to promotin, bigger bonus

	// position value tables and piece values taken from
	// http://chessprogramming.wikispaces.com/Simplified+evaluation+function
	final val positionValue = Array(
		// black pawns
		Array(
			0,  0,  0,  0,  0,  0,  0,  0,
			50, 50, 50, 50, 50, 50, 50, 50,
			10, 10, 20, 30, 30, 20, 10, 10,
			5,  5, 10, 27, 27, 10,  5,  5,
			0,  0,  0, 25, 25,  0,  0,  0,
			5, -5,-10,  0,  0,-10, -5,  5,
			5, 10, 10,-25,-25, 10, 10,  5,
			0,  0,  0,  0,  0,  0,  0,  0),
		// white pawns
		Array(
			0,  0,  0,  0,  0,  0,  0,  0,
			5, 10, 10,-25,-25, 10, 10,  5,
			5, -5,-10,  0,  0,-10, -5,  5,
			0,  0,  0, 25, 25,  0,  0,  0,
			5,  5, 10, 27, 27, 10,  5,  5,
			10, 10, 20, 30, 30, 20, 10, 10,
			50, 50, 50, 50, 50, 50, 50, 50,
			0,  0,  0,  0,  0,  0,  0,  0)
		)

	// array containg pawn move direction, indexed by color
	final val moveDirection = Array(-10, 10)
	
	// stores information on which rank pawn of given color gets promoted
	final val promotionRank = Array(0, 7)

	// on which row pawn on given color starts
	final val startingRow = Array(6, 1)

	final val attackDirections = Array(Array(-9, -11), Array(9, 11))
}
