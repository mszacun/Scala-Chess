package src;

class Board(fen : String = "") 
{
	// TODO: Implement fen parsing

	// board 10 x 12 representation with auxilary fields
	val board : Array[Int] = new Array[Int](10 * 12)

	// list of all pieces, indexed by keys defined below
	val piecesList : Array[Piece] = new Array[Piece](32)

	/* 
	 * castlingRights[0] -> white ability to castle kingside
	 * castlingRights[1] -> white ability to castle queenside
	 * castlingRights[2] -> black ability to castle kingside
	 * castlingRights[3] -> black ability to castle queenside
	 * castlingRights[4] -> this is set by rook that during move isn't on starting
	 *	position
	 */
	var castlingRights : Seq[Boolean] = Array(true, true, true, true, false)

	// fields from which enPassant capture is possible
	var enPassant1 : Int = 0
	var enPassant2 : Int = 0

	// history of moves on board in stack form
	var movesStack : List[Move] = Nil

	/* empty board */
	for	(i <- 0 to 119)
	{
		if (i < 20 || i > 100 || i % 10 == 0 || i % 10 == 9)
			board(i) = Board.AUXILIARY_SQUARE
		else
			board(i) = Board.EMPTY_SQUARE
	}

	def addPiece(piece : Piece) = 
	{
		board(piece.position) = piece.id
		piecesList(piece.id) = piece
	}

	// this method should be called instead of Move.apply!
	def makeMove(m : Move) = 
	{
		movesStack = m :: movesStack
		m.apply(this)
	}

	// reverts last move, may throw an exception if moves stack is empty
	def undoMove() = 
	{
		val moveToUndo = movesStack.head
		movesStack = movesStack.tail
		moveToUndo.undo(this)
	}

	// checks wheter opponent can attack this field, used in looking for check
	def isAttacked(position : Int, myColor : Int) : Boolean =
	{
		// check for rook and queen moving straight
		var tmpPos = position
		for (dir <- Rook.possibleDirections)
		{
			tmpPos = position + dir
			while (isEmpty(tmpPos)) // ommit all empty squares
				tmpPos += dir
			if (isOccupiedByOpponent(tmpPos, myColor) &&	
				(piecesList(board(tmpPos)).pieceType == Piece.ROOK ||
				piecesList(board(tmpPos)).pieceType == Piece.QUEEN))
				return true
		}

		// check for bishop or queen moving crosswise
		tmpPos = position
		for (dir <- Bishop.possibleDirections)
		{
			tmpPos = position + dir
			while (isEmpty(tmpPos)) // ommit all empty squares
				tmpPos += dir
			if (isOccupiedByOpponent(tmpPos, myColor) &&	
				(piecesList(board(tmpPos)).pieceType == Piece.BISHOP ||
				piecesList(board(tmpPos)).pieceType == Piece.QUEEN))
				return true
		}

		// check for black powns
		if (myColor == Piece.WHITE)
		{
			// where pawn must be to attack this square
			val possibilities = Cord.moveSE(position, 1) :: Cord.moveSW(position, 1) :: Nil
			
			// check is on any of this square is opponent piece and this piece is pawn
			if (possibilities.exists((x : Int) =>
				isOccupiedByOpponent(x, myColor) && 
					piecesList(board(x)).pieceType == Piece.PAWN
				))
				return true
		}
		// check for white pawn
		else
		{
			// where pawn must be to attack this square
			val possibilities = Cord.moveNE(position, 1) :: Cord.moveNW(position, 1) :: Nil

			if (possibilities.exists((x : Int) =>
				isOccupiedByOpponent(x, myColor) && 
					piecesList(board(x)).pieceType == Piece.PAWN
				))
				return true
		}

		// check for knight
		var possibilities = Knight.possibleMovesDirection.map((dir : Int) => 
			position + dir)

		if (possibilities.exists((x : Int) =>
			isOccupiedByOpponent(x, myColor) && 
				piecesList(board(x)).pieceType == Piece.KNIGHT
			))
			return true

		// check for king
		possibilities = King.possibleDirections.map((dir : Int) => 
			position + dir)

		if (possibilities.exists((x : Int) =>
			isOccupiedByOpponent(x, myColor) && 
				piecesList(board(x)).pieceType == Piece.KING
			))
			return true

		return false

	}

	// check if color king is in check
	def isCheck(color : Int) = 
	{
		val king = piecesList(if (color == Piece.WHITE) Board.WHITE_KING else Board.BLACK_KING)
		isAttacked(king.position, color)
	}

	// WARNING: if you use one of those methods, make sure, you've choosen the
	// right one, becasue there are no 2 like in boolean, but 3(empty, occupied, 
	// off the board)!!

	def isEmpty(position : Int) = board(position) == Board.EMPTY_SQUARE

	def isOccupied(position : Int) = board(position) >= 0

	// quickly checks color of piece
	def isOccupiedByOpponent(position : Int, myColor : Int) = 
		isOccupied(position) && (board(position) & 1) != myColor

	def isOffBoard(position : Int) = board(position) == Board.AUXILIARY_SQUARE
}	

object Board
{
	val AUXILIARY_SQUARE = -1
	val EMPTY_SQUARE = -2
	// keys in piece list

	// white's are even
	val WHITE_PAWN_1 = 1
	val WHITE_PAWN_2 = 3
	val WHITE_PAWN_3 = 5
	val WHITE_PAWN_4 = 7
	val WHITE_PAWN_5 = 9
	val WHITE_PAWN_6 = 11
	val WHITE_PAWN_7 = 13
	val WHITE_PAWN_8 = 15
	
	val WHITE_ROOK_1 = 17
	val WHITE_ROOK_2 = 19
	val WHITE_KNIGHT_1 = 21
	val WHITE_KNIGHT_2 = 23
	val WHITE_BISHOP_1 = 25
	val WHITE_BISHOP_2 = 27
	val WHITE_QUEEN = 29
	val WHITE_KING = 31

	// blacks are odd, this is used in fast check of piece color on certain square
	val BLACK_PAWN_1 = 0
	val BLACK_PAWN_2 = 2
	val BLACK_PAWN_3 = 4
	val BLACK_PAWN_4 = 6
	val BLACK_PAWN_5 = 8
	val BLACK_PAWN_6 = 10
	val BLACK_PAWN_7 = 12
	val BLACK_PAWN_8 = 14
	
	val BLACK_ROOK_1 = 16
	val BLACK_ROOK_2 = 18
	val BLACK_KNIGHT_1 = 20
	val BLACK_KNIGHT_2 = 22
	val BLACK_BISHOP_1 = 24
	val BLACK_BISHOP_2 = 26
	val BLACK_QUEEN = 28
	val BLACK_KING = 30
}
