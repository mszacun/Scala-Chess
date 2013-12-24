package src;

import scala.collection.mutable.Map

class Board() 
{
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
	var enPassant1 : Int = 0 // in west direction
	var enPassant2 : Int = 0 // in east direction

	// history of moves on board in stack form
	var movesStack : List[Move] = Nil
	var whoseMove = Piece.WHITE

	clearBoard

	/* empty board */
	def clearBoard =
	{
		for	(i <- 0 to 119)
		{
			if (i < 20 || i > 100 || i % 10 == 0 || i % 10 == 9)
				board(i) = Board.AUXILIARY_SQUARE
			else
				board(i) = Board.EMPTY_SQUARE
		}
		for (i <- 1 until 32) piecesList(i) = null
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

		// revert enPassants and castlingRights
		val previousMove : Move = movesStack match
			{
				case h :: t => h
				// at the beginning everyone can castle
				case Nil => new QuietMove(0, 0, 0, 0, Array(true, true, true,
					true, false))
			}
		castlingRights = previousMove.castlingRightsAfter
		enPassant1 = previousMove.enPassant1
		enPassant2 = previousMove.enPassant2
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
				(Board.isRook(board(tmpPos)) || Board.isQueen(board(tmpPos))))
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
				(Board.isBishop(board(tmpPos)) || Board.isQueen(board(tmpPos))))
				return true
		}

		// check for black powns
		if (myColor == Piece.WHITE)
		{
			// where pawn must be to attack this square
			val possibilities = Cord.moveSE(position, 1) :: Cord.moveSW(position, 1) :: Nil
			
			// check is on any of this square is opponent piece and this piece is pawn
			if (possibilities.exists((x : Int) =>
				isOccupiedByOpponent(x, myColor) && Board.isPawn(board(x))))
				return true
		}
		// check for white pawn
		else
		{
			// where pawn must be to attack this square
			val possibilities = Cord.moveNE(position, 1) :: Cord.moveNW(position, 1) :: Nil

			if (possibilities.exists((x : Int) =>
				isOccupiedByOpponent(x, myColor) && Board.isPawn(board(x))))
				return true
		}

		// check for knight
		var possibilities = Knight.possibleMovesDirection.map((dir : Int) => 
			position + dir)

		if (possibilities.exists((x : Int) =>
			isOccupiedByOpponent(x, myColor) && Board.isKnight(board(x))))
			return true

		// check for king
		possibilities = King.possibleDirections.map((dir : Int) => 
			position + dir)

		if (possibilities.exists((x : Int) =>
			isOccupiedByOpponent(x, myColor) && Board.isKing(board(x))))
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

	// array indexed by piece key, telling if given piece is pawn
	val isPawn = Array(true, true, true, true, true, true, true, true, true, true,
						true, true, true, true, true, true, false, false, false, false,
						false, false, false, false, false, false, false, false, false, false,
						false, false)
	val isKnight = Array(false, false, false, false, false, false, false, false, false, false,
						false, false, false, false, false, false, false, false, false, false,
						true, true, true, true, false, false, false, false, false, false,
						false, false)
	val isRook = Array(false, false, false, false, false, false, false, false, false, false,
						false, false, false, false, false, false, true, true, true, true,
						false, false, false, false, false, false, false, false, false, false,
						false, false)
	val isBishop = Array(false, false, false, false, false, false, false, false, false, false,
						false, false, false, false, false, false, false, false, false, false,
						false, false, false, false, true, true, true, true, false, false,
						false, false)
	val isQueen = Array(false, false, false, false, false, false, false, false, false, false,
						false, false, false, false, false, false, false, false, false, false,
						false, false, false, false, false, false, false, false, true, true,
						false, false)
	val isKing = Array(false, false, false, false, false, false, false, false, false, false,
						false, false, false, false, false, false, false, false, false, false,
						false, false, false, false, false, false, false, false, false, false,
						true, true)


	def apply(fen : String) =
	{
		val board = new Board()
		var stringIndex = 0
		var arrayPos = Cord.fromString("A8")

		// store information how much of certain type of pieces we lack 
		// from starting state
		val nextPieceKey = Map('r' -> 1, 'n' -> 1, 'b' -> 1, 'q' -> 0, 'k' -> 0,
			'p' -> 7, 'R' -> 1, 'N' -> 1, 'B' -> 1, 'Q' -> 0, 'K' -> 0, 'P' -> 7)
		val piecesKeys = Map(
			'r' -> Array(BLACK_ROOK_2, BLACK_ROOK_1),
			'n' -> Array(BLACK_KNIGHT_2, BLACK_KNIGHT_1),
			'b' -> Array(BLACK_BISHOP_2, BLACK_BISHOP_1),
			'q' -> Array(BLACK_QUEEN),
			'k' -> Array(BLACK_KING),
			'p' -> Array(BLACK_PAWN_8, BLACK_PAWN_7, BLACK_PAWN_6, BLACK_PAWN_5,
				BLACK_PAWN_4, BLACK_PAWN_3, BLACK_PAWN_2, BLACK_PAWN_1),
			'R' -> Array(WHITE_ROOK_2, WHITE_ROOK_1),
			'N' -> Array(WHITE_KNIGHT_2, WHITE_KNIGHT_1),
			'B' -> Array(WHITE_BISHOP_2, WHITE_BISHOP_1),
			'Q' -> Array(WHITE_QUEEN),
			'K' -> Array(WHITE_KING),
			'P' -> Array(WHITE_PAWN_8, WHITE_PAWN_7, WHITE_PAWN_6, WHITE_PAWN_5,
				WHITE_PAWN_4, WHITE_PAWN_3, WHITE_PAWN_2, WHITE_PAWN_1))

		while (fen.charAt(stringIndex) != ' ')
		{
			val char : Char = fen.charAt(stringIndex)
			if (char == '/') arrayPos -= 18 // got to next row
			else if (Character.isDigit(char)) arrayPos += char.toInt - '0'.toInt
			// have to put new piece
			else 
			{
				var pieceID = 0
				if (nextPieceKey(char) >= 0)
				{
					val index : Int = nextPieceKey(char)
					pieceID = piecesKeys(char)(index)
					nextPieceKey(char) -= 1
				}
				// there was promotion during game
				else
				{
					val pawnLetter = if (Character.isUpperCase(char)) 'P' else 'p'
					val index : Int = nextPieceKey(pawnLetter)
					pieceID = piecesKeys(pawnLetter)(index)
					nextPieceKey(pawnLetter) = index - 1
				}
				val newPiece = char match
				{
					case 'r' => new Rook(arrayPos, Piece.BLACK, pieceID)
					case 'R' => new Rook(arrayPos, Piece.WHITE, pieceID)
					case 'n' => new Knight(arrayPos, Piece.BLACK, pieceID)
					case 'N' => new Knight(arrayPos, Piece.WHITE, pieceID)
					case 'b' => new Bishop(arrayPos, Piece.BLACK, pieceID)
					case 'B' => new Bishop(arrayPos, Piece.WHITE, pieceID)
					case 'q' => new Queen(arrayPos, Piece.BLACK, pieceID)
					case 'Q' => new Queen(arrayPos, Piece.WHITE, pieceID)
					case 'k' => new King(arrayPos, Piece.BLACK, pieceID)
					case 'K' => new King(arrayPos, Piece.WHITE, pieceID)
					case 'p' => new Pawn(arrayPos, Piece.BLACK, pieceID)
					case 'P' => new Pawn(arrayPos, Piece.WHITE, pieceID)
				}
				board.addPiece(newPiece)
				arrayPos += 1
			}
			stringIndex += 1
		}
		stringIndex += 1 // ommit space
		board.whoseMove = if (fen.charAt(stringIndex) == 'w') Piece.WHITE
						  else 								  Piece.BLACK

		stringIndex += 2 
		val castlingRights = Array(false, false, false, false, false)
		while (fen.charAt(stringIndex) != ' ')
		{
			fen.charAt(stringIndex) match
			{
				case 'K' => castlingRights(0) = true
				case 'Q' => castlingRights(1) = true
				case 'k' => castlingRights(2) = true
				case 'q' => castlingRights(3) = true
			}
			stringIndex += 1
		}
		board.castlingRights = castlingRights

		// TODO: restoring en passant nor supported!
		board.enPassant1 = 0
		board.enPassant2 = 0

		// TODO: Halfmove and Fullmove clocks not supported

		board
	} 
}
