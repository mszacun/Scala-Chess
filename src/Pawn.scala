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
		if (color == Piece.WHITE)
		{
			// default move
			var move = Cord.moveS(position, 1)
			if (b.isEmpty(move) && !b.isOffBoard(move))
			{
				if (Cord.getRow(move) == 7)
					Pawn.possiblePromotions.foreach((pieceType : Int) =>
					{
						moveList(index) = new PromotionMove(position, move,
							b.castlingRights, pieceType)
						index += 1
					})
				else
				{
					moveList(index) = new QuietMove(position, move, 0, b.castlingRights)
					index += 1

					// double move if on starting position
					if (Cord.getRow(position) == whitePawnsStartingRow)
					{
						move = Cord.moveS(position, 2)
						if (b.isEmpty(move) && !b.isOffBoard(move))
						{
							moveList(index) = new QuietMove(position, 
								move, Cord.moveS(position, 1), // enPasant is possible
								b.castlingRights)
							index += 1
						}
					}
				}
			}
		}
		else
		{
			// default move
			var move = Cord.moveN(position, 1)
			if (b.isEmpty(move) && !b.isOffBoard(move))
			{
				if (Cord.getRow(move) == 0)
					Pawn.possiblePromotions.foreach((pieceType : Int) =>
					{
						moveList(index) = new PromotionMove(position, move,
							b.castlingRights, pieceType)
						index += 1
					})
				else
				{
					moveList(index) = new QuietMove(position, move, 0, b.castlingRights)
					index += 1

					// double move if on starting position
					if (Cord.getRow(position) == blackPawnsStartingRow)
					{
						move = Cord.moveN(position, 2)
						if (b.isEmpty(move) && !b.isOffBoard(move))
						{
							moveList(index) = new QuietMove(position, 
								move, Cord.moveN(position, 1),
								b.castlingRights)
							index += 1
						}
					}
				}
			}
		}
		index
	}

	def generateAttacks(b : Board, moveList : Array[Move], ind : Int) :Int =
	{
		var index = ind
		var possibleAttacks : List[Int] = Nil
		var lastRow : Int = 0 // row on which pawn of given color will be promoted
		if (color == Piece.WHITE)
		{
			lastRow = 7
			possibleAttacks = Cord.moveSE(position, 1) :: 
				Cord.moveSW(position, 1) :: Nil
		}
		else
		{
			lastRow = 0
			possibleAttacks = Cord.moveNE(position, 1) :: 
				Cord.moveNW(position, 1) :: Nil
		}
		possibleAttacks.foreach((pos : Int) =>
		{
			if (b.isOccupiedByOpponent(pos, color))
				if (Cord.getRow(pos) == lastRow)
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
		index
	}

	override def generateMoves(b : Board, moveList:Array[Move], index : Int) : Int =
	{
		val ind = generateAttacks(b, moveList, index)
		generateQuietMoves(b, moveList, ind)
	}

	def rank(b : Board) = Pawn.pieceValue + 
		Pawn.positionValue(color)(Cord.from120to64(position))

}

object Pawn
{
	val possiblePromotions = Piece.KNIGHT :: Piece.BISHOP :: Piece.QUEEN ::
		Piece.ROOK :: Nil

	val pieceValue = 100

	val positionValue = Array(
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
}
