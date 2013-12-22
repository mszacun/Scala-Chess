package src;


class Pawn(position : Int, color : Boolean, id : Int) 
	extends Piece(position, color, id)
{
	val whitePawnsStartingRow = 1 // remeber, that rows are 0 indexed
	val blackPawnsStartingRow = 6

	def this(position : String, color : Boolean, id : Int)= 
		this(Cord.fromString(position), color, id)

	// see in Piece
	def generateQuietMoves(b : Board) : Traversable[Move] =
	{
		var result : List[Move]= Nil

		if (color)
		{
			// default move
			if (b.isEmpty(Cord.moveS(position, 1)))
			{
				result = new QuietMove(position,
					Cord.moveS(position, 1), 0, 0,
					b.castlingRights) :: result

				// double move if on starting position
				if (Cord.getRow(position) == whitePawnsStartingRow)
					if (b.isEmpty(Cord.moveS(position, 2)))
						result = new QuietMove(position, 
							Cord.moveS(position, 2), 
							Cord.moveSW(position, 1), Cord.moveSE(position, 1),
							b.castlingRights) :: result
			}
		}
		else
		{
			// default move
			if (b.isEmpty(Cord.moveN(position, 1)))
			{
				result = new QuietMove(position,
					Cord.moveN(position, 1), 0, 0,
					b.castlingRights) :: result

				// double move if on starting position
				if (Cord.getRow(position) == blackPawnsStartingRow)
					if (b.isEmpty(Cord.moveN(position, 2)))
						result = new QuietMove(position, 
							Cord.moveN(position, 2),
							Cord.moveNW(position, 1), Cord.moveNE(position, 1),
							b.castlingRights) :: result
			}
		}
		result
	}

	def generateAttacks(b : Board) : Traversable[Move] =
	{
/*		var result : List[Move] = Nil
		if (color)
		{
			if (b.isEmpty(Cord.moveSE(position, 1)))
				result = new QuietMove(position, Cord.moveSE(position, 1), 0, 0,
				b.castlingRights) :: result
			if (b.isEmpty(Cord.moveSW(position, 1)))
				result = new QuietMove(position, Cord.moveSW(position, 1), 0, 0,
				b.castlingRights) :: result
		}
		else
		{
			if (b.isEmpty(Cord.moveNE(position, 1)))
				result = new QuietMove(position, Cord.moveNE(position, 1), 0, 0,
				b.castlingRights) :: result
			if (b.isEmpty(Cord.moveNW(position, 1)))
				result = new QuietMove(position, Cord.moveNW(position, 1), 0, 0,
				b.castlingRights) :: result
		}
		result */
		Nil
	}
	def generateMoves(b : Board) : Traversable[Move] =
		generateAttacks(b) ++ generateQuietMoves(b)

	/* TODO: Implement */
	def rank = 0

}

