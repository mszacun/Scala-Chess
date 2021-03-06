package src;

import scala.collection.mutable.Map
import scala.collection.mutable.MutableList

class Board() 
{
	// board 10 x 12 representation with auxilary fields
	val board : Array[Int] = new Array[Int](10 * 12)

	// list of all pieces, indexed by keys defined below
	val piecesList : Array[Piece] = new Array[Piece](32)

	/* 
	 * stored on bits of integer
	 * castlingRights[0] -> white ability to castle kingside
	 * castlingRights[1] -> white ability to castle queenside
	 * castlingRights[2] -> black ability to castle kingside
	 * castlingRights[3] -> black ability to castle queenside
	 * castlingRights[4] -> this is set by rook that during move isn't on starting
	 *	position
	 */
	var castlingRights : Int = 1 | 2 | 4 | 8

	// fields from which enPassant capture is possible
	var enPassant : Int = 0 // target en passant square

	// history of moves on board in stack form
	var movesStack : List[Move] = new QuietMove(0, 0, 0, 15, false) :: Nil // no need to check if stack is empty
	// board hash history since last 
	val boardHashHistorySize = 128
	val boardHashHistory = new Array[Long](boardHashHistorySize) 
	var lastUndoAbleMove = 0 // index in board history, for example pawn move or capture
	// number of moves in history
	var boardHashHistoryIndex = 0 // index, where last hash was put
	var whoseMove = Piece.WHITE

	// number of alive pieces
	var numberOfPiecesAlive = 0

	// score for players
	val scores = Array[Int](0, 0)

	// hash value of current state of board
	var boardHash : Long = 0

	clearBoard

	/* empty board */
	final def clearBoard =
	{
		for	(i <- 0 to 119)
		{
			if (i < 20 || i > 100 || i % 10 == 0 || i % 10 == 9)
				board(i) = Board.AUXILIARY_SQUARE
			else
				board(i) = Board.EMPTY_SQUARE
		}
		for (i <- 1 until 32) piecesList(i) = null
		numberOfPiecesAlive = 0
	}

	// WARNING: Generates also pseudolegal moves
	final def generateMovesForNextPlayer =
	{
		var start = System.nanoTime
		val result = new Array[Move](256)
		var i = 0 // index in result array

		// check if castles are possible
		if (whoseMove == Piece.WHITE)
		{
			if (!isAttacked(piecesList(Board.WHITE_KING).position, whoseMove))
			{
				val castleRightsAfter = 12 // white lose castling rights

				// castle king side
				if ((castlingRights & 1) == 1 && Board.isRook(board(Board.whiteRookKSStartPos)) &&
					Board.freeSquaresRequiredWhiteCastleKS.forall((sq : Int) => 
						isEmpty(sq) && !isAttacked(sq, whoseMove)))
				{
					result(i) = new CastleMove(Board.whiteRookKSStartPos, Board.whiteRookKSEndPos,
						Board.whiteKingStartPos, Board.whiteKingKSEndPos, castleRightsAfter)
					i += 1
				}

				// castle queen side
				if ((castlingRights & 2) == 2 && Board.isRook(board(Board.whiteRookQSStartPos)) &&
					isEmpty(Board.additionalFreeSquareWhiteCastleQS) &&
					Board.freeSquaresRequiredWhiteCastleQS.forall((sq : Int) =>
						isEmpty(sq) && !isAttacked(sq, whoseMove)))
				{
					result(i) = new CastleMove(Board.whiteRookQSStartPos, Board.whiteRookQSEndPos,
						Board.whiteKingStartPos, Board.whiteKingQSEndPos, castleRightsAfter)
					i += 1
				}
			}
		}
		else
		{
			if (!isAttacked(piecesList(Board.BLACK_KING).position, whoseMove))
			{
				val castleRightsAfter = 3 // black lose castling rights

				// castle king side
				if ((castlingRights & 4) == 4 && Board.isRook(board(Board.blackRookKSStartPos)) &&
					Board.freeSquaresRequiredBlackCastleKS.forall((sq : Int) =>
						isEmpty(sq) && !isAttacked(sq, whoseMove)))
				{
					result(i) = new CastleMove(Board.blackRookKSStartPos, Board.blackRookKSEndPos,
						Board.blackKingStartPos, Board.blackKingKSEndPos, castleRightsAfter)
					i += 1
				}
				// castle queen side
				if ((castlingRights & 8) == 8 && Board.isRook(board(Board.blackRookQSStartPos)) &&
					isEmpty(Board.additionalFreeSquareBlackCastleQS) &&
					Board.freeSquaresRequiredBlackCastleQS.forall((sq : Int) =>
						isEmpty(sq) && !isAttacked(sq, whoseMove)))
				{
					result(i) = new CastleMove(Board.blackRookQSStartPos, Board.blackRookQSEndPos,
						Board.blackKingStartPos, Board.blackKingQSEndPos, castleRightsAfter)
					i += 1
				}
			}
		}

		// check if en passant is possible
		if (!isOffBoard(enPassant))
		{
			if (whoseMove == Piece.WHITE)
			{
				// on the left
				var pawnSquare = enPassant - 9
				if (isOccupiedByMe(pawnSquare, whoseMove) && 
					Board.isPawn(board(pawnSquare)))
				{
					result(i) = new EnPassantMove(pawnSquare, enPassant, pawnSquare - 1,
						castlingRights)
					i += 1
				}
				pawnSquare = enPassant - 11
				// on the right
				if (isOccupiedByMe(pawnSquare, whoseMove) && 
					Board.isPawn(board(pawnSquare)))
				{
					result(i) = new EnPassantMove(pawnSquare, enPassant, pawnSquare + 1,
						castlingRights)
					i += 1
				}
			}
			else
			{
				var pawnSquare = enPassant + 9
				if (isOccupiedByMe(pawnSquare, whoseMove) && 
					Board.isPawn(board(pawnSquare)))
				{
					result(i) = new EnPassantMove(pawnSquare, enPassant, pawnSquare + 1,
						castlingRights)
					i += 1
				}

				pawnSquare = enPassant + 11
				if (isOccupiedByMe(pawnSquare, whoseMove) && 
					Board.isPawn(board(pawnSquare)))
				{
					result(i) = new EnPassantMove(pawnSquare, enPassant, pawnSquare - 1,
						castlingRights)
					i += 1
				}
			}
		}

		// generate quiet moves, captures and promotions
		piecesList.foreach((piece : Piece) =>
		{
			if (piece != null && piece.color == whoseMove)
			{
				i = piece.generateMoves(this, result, i)
			}
		})
		val end = System.nanoTime
		(result, i)
	}

	// this method is slow, shouldn't be use in search
	final def generateValidMovesForNextPlayer : Array[Move] = 
	{
		generateMovesForNextPlayer._1.filter(m =>
		{
			var result = false

			if (m != null)
			{
				result = makeMove(m)
				undoMove
			}
			result
		})
	}

	// generates captures
	final def generateAttacksForNextPlayer =
	{
		val result = new Array[Move](256)
		var i = 0 // index in result array

		// check if en passant is possible
		if (!isOffBoard(enPassant))
		{
			if (whoseMove == Piece.WHITE)
			{
				// on the left
				var pawnSquare = enPassant - 9
				if (isOccupiedByMe(pawnSquare, whoseMove) && 
					Board.isPawn(board(pawnSquare)))
				{
					result(i) = new EnPassantMove(pawnSquare, enPassant, pawnSquare - 1,
						castlingRights)
					i += 1
				}
				pawnSquare = enPassant - 11
				// on the right
				if (isOccupiedByMe(pawnSquare, whoseMove) && 
					Board.isPawn(board(pawnSquare)))
				{
					result(i) = new EnPassantMove(pawnSquare, enPassant, pawnSquare + 1,
						castlingRights)
					i += 1
				}
			}
			else
			{
				var pawnSquare = enPassant + 9
				if (isOccupiedByMe(pawnSquare, whoseMove) && 
					Board.isPawn(board(pawnSquare)))
				{
					result(i) = new EnPassantMove(pawnSquare, enPassant, pawnSquare + 1,
						castlingRights)
					i += 1
				}

				pawnSquare = enPassant + 11
				if (isOccupiedByMe(pawnSquare, whoseMove) && 
					Board.isPawn(board(pawnSquare)))
				{
					result(i) = new EnPassantMove(pawnSquare, enPassant, pawnSquare - 1,
						castlingRights)
					i += 1
				}
			}
		}

		// generate quiet moves, captures and promotions
		piecesList.foreach((piece : Piece) =>
		{
			if (piece != null && piece.color == whoseMove)
			{	
				i = piece.generateAttacks(this, result, i)
			}
		})
		(result, i)
	}

	final def addPiece(piece : Piece) = 
	{
		board(piece.position) = piece.id
		piecesList(piece.id) = piece
	}

	final def getHalfMoveClock = boardHashHistoryIndex - lastUndoAbleMove


	// this method should be called instead of Move.apply!
	// returns if given move is legal
	final def makeMove(m : Move) : Boolean = 
	{
		boardHashHistoryIndex += 1
		movesStack = m :: movesStack
		m.apply(this)

		
		whoseMove ^= 1 // hacker style to switch player :)

		// update and store hash
		updateBoardHash
		updateScores

		boardHashHistory(boardHashHistoryIndex) = boardHash
		
		return !isCheck(whoseMove ^ 1)
	}

	// reverts last move, may throw an exception if moves stack is empty
	final def undoMove() = 
	{
		val before = getPlayerScore(Piece.BLACK)
		val moveToUndo = movesStack.head
		movesStack = movesStack.tail
		moveToUndo.undo(this)
		boardHashHistoryIndex -= 1


		// revert enPassants and castlingRights
		val previousMove : Move = movesStack.head
		castlingRights = previousMove.castlingRightsAfter
		enPassant = previousMove.enPassant

		whoseMove ^= 1 // hacker style to switch player :)
		updateBoardHash // think about remove this
		updateScores

		boardHashHistory(boardHashHistoryIndex) = boardHash // or this
	}

	final def updateScores = 
	{
		scores(0) = 0
		scores(1) = 0
		for (piece <- piecesList)
			if (piece != null)
				scores(piece.color) += piece.rank(this)
	}

	final def updateBoardHash = 
	{
		boardHash = 0

		if (whoseMove == Piece.WHITE)
			boardHash ^= Hash.whiteMovesHash
		
		boardHash ^= Hash.enPassantSquareHash(Cord.from120to64(enPassant))

		boardHash ^= Hash.castleRightsHash(castlingRights)

		piecesList.foreach((p : Piece) =>
			if (p != null) boardHash ^= p.hashKey)
	}

	final def countRepetitions = 
	{
		var i = lastUndoAbleMove
		var repetition = 0
		while (i <= boardHashHistoryIndex)
		{
			if (boardHashHistory(i) == boardHash)
				repetition += 1
			i += 1
		}
		repetition
	}

	final def getPlayerScore(player : Int) = scores(player) - scores(player ^ 1)

	// checks wheter opponent can attack this field, used in looking for check
	final def isAttacked(position : Int, myColor : Int) : Boolean =
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
	final def isCheck(color : Int) = 
	{
		val king = piecesList(if (color == Piece.WHITE) Board.WHITE_KING else Board.BLACK_KING)
		isAttacked(king.position, color)
	}

	// WARNING: if you use one of those methods, make sure, you've choosen the
	// right one, becasue there are no 2 like in boolean, but 3(empty, occupied, 
	// off the board)!!

	final def isEmpty(position : Int) = board(position) == Board.EMPTY_SQUARE

	final def isOccupied(position : Int) = board(position) < 32

	// quickly checks color of piece
	final def isOccupiedByOpponent(position : Int, myColor : Int) = 
		isOccupied(position) && (board(position) & 1) != myColor

	final def isOccupiedByMe(position : Int, myColor : Int) = 
		isOccupied(position) && (board(position) & 1) == myColor

	final def isOffBoard(position : Int) = board(position) == Board.AUXILIARY_SQUARE

	def toFen =
	{
		var builder = new StringBuilder()
		var spaceBetweenPieces = 0
		for (row <- (1 to 8).reverse)
		{
			for (col <- 'A' to 'H')
			{
				val pos = Cord.fromString(col.toString + row)
				if (isEmpty(pos))
					spaceBetweenPieces += 1
				else
				{
					if (spaceBetweenPieces > 0)
					{
						builder.append(spaceBetweenPieces)
						spaceBetweenPieces = 0
					}
					builder.append(Board.pieceKeyToFEN(piecesList(board(pos)).id))
				}
			}
			if (spaceBetweenPieces > 0)
			{
				builder.append(spaceBetweenPieces)
				spaceBetweenPieces = 0
			}
			// on the end of piece position description there is no '/'
			if (row > 1) builder.append('/')
		}

		val playerChar = if (whoseMove == Piece.WHITE) 'w' else 'b'
		builder.append(" " + playerChar + " ")

		// castlingRights
		if ((castlingRights & 1) == 1) builder.append('K')
		if ((castlingRights & 2) == 2) builder.append('Q')
		if ((castlingRights & 4) == 4) builder.append('k')
		if ((castlingRights & 8) == 8) builder.append('q')
		// no castle rights ?
		if ((castlingRights & 15) == 0)
			builder.append("-")

		// enPassant
		builder.append(" ")

		if (!isOffBoard(enPassant))
			builder.append(Cord.toString(enPassant))
		else
			builder.append("-")

		// half-move clock
		builder.append(" " + getHalfMoveClock)

		builder.toString
	}
}	

object Board
{
	Hash.initHashTables

	final val AUXILIARY_SQUARE = 33
	final val EMPTY_SQUARE = 32
	// keys in piece list

	// white's are even
	final val WHITE_PAWN_1 = 1
	final val WHITE_PAWN_2 = 3
	final val WHITE_PAWN_3 = 5
	final val WHITE_PAWN_4 = 7
	final val WHITE_PAWN_5 = 9
	final val WHITE_PAWN_6 = 11
	final val WHITE_PAWN_7 = 13
	final val WHITE_PAWN_8 = 15
	
	final val WHITE_ROOK_1 = 17
	final val WHITE_ROOK_2 = 19
	final val WHITE_KNIGHT_1 = 21
	final val WHITE_KNIGHT_2 = 23
	final val WHITE_BISHOP_1 = 25
	final val WHITE_BISHOP_2 = 27
	final val WHITE_QUEEN = 29
	final val WHITE_KING = 31

	// blacks are odd, this is used in fast check of piece color on certain square
	final val BLACK_PAWN_1 = 0
	final val BLACK_PAWN_2 = 2
	final val BLACK_PAWN_3 = 4
	final val BLACK_PAWN_4 = 6
	final val BLACK_PAWN_5 = 8
	final val BLACK_PAWN_6 = 10
	final val BLACK_PAWN_7 = 12
	final val BLACK_PAWN_8 = 14
	
	final val BLACK_ROOK_1 = 16
	final val BLACK_ROOK_2 = 18
	final val BLACK_KNIGHT_1 = 20
	final val BLACK_KNIGHT_2 = 22
	final val BLACK_BISHOP_1 = 24
	final val BLACK_BISHOP_2 = 26
	final val BLACK_QUEEN = 28
	final val BLACK_KING = 30

	// array indexed by piece key, telling if given piece is pawn
	final val isPawn = Array(true, true, true, true, true, true, true, true, true, true,
						true, true, true, true, true, true, false, false, false, false,
						false, false, false, false, false, false, false, false, false, false,
						false, false, false, false)
	final val isKnight = Array(false, false, false, false, false, false, false, false, false, false,
						false, false, false, false, false, false, false, false, false, false,
						true, true, true, true, false, false, false, false, false, false,
						false, false, false, false)
	final val isRook = Array(false, false, false, false, false, false, false, false, false, false,
						false, false, false, false, false, false, true, true, true, true,
						false, false, false, false, false, false, false, false, false, false,
						false, false, false, false)
	final val isBishop = Array(false, false, false, false, false, false, false, false, false, false,
						false, false, false, false, false, false, false, false, false, false,
						false, false, false, false, true, true, true, true, false, false,
						false, false, false, false)
	final val isQueen = Array(false, false, false, false, false, false, false, false, false, false,
						false, false, false, false, false, false, false, false, false, false,
						false, false, false, false, false, false, false, false, true, true,
						false, false, false, false)
	final val isKing = Array(false, false, false, false, false, false, false, false, false, false,
						false, false, false, false, false, false, false, false, false, false,
						false, false, false, false, false, false, false, false, false, false,
						true, true, false, false)
	final val pieceKeyToFEN = Map(WHITE_PAWN_1 -> 'P', WHITE_PAWN_2 -> 'P',
		WHITE_PAWN_3 -> 'P', WHITE_PAWN_4 -> 'P', WHITE_PAWN_5 -> 'P',
		WHITE_PAWN_6 -> 'P', WHITE_PAWN_7 -> 'P', WHITE_PAWN_8 -> 'P',
		WHITE_ROOK_1 -> 'R', WHITE_ROOK_2 -> 'R', WHITE_KNIGHT_1 -> 'N',
		WHITE_KNIGHT_2 -> 'N', WHITE_BISHOP_1 -> 'B', WHITE_BISHOP_2 -> 'B',
		WHITE_QUEEN -> 'Q', WHITE_KING -> 'K',
		BLACK_PAWN_1 -> 'p', BLACK_PAWN_2 -> 'p',
		BLACK_PAWN_3 -> 'p', BLACK_PAWN_4 -> 'p', BLACK_PAWN_5 -> 'p',
		BLACK_PAWN_6 -> 'p', BLACK_PAWN_7 -> 'p', BLACK_PAWN_8 -> 'p',
		BLACK_ROOK_1 -> 'r', BLACK_ROOK_2 -> 'r', BLACK_KNIGHT_1 -> 'n',
		BLACK_KNIGHT_2 -> 'n', BLACK_BISHOP_1 -> 'b', BLACK_BISHOP_2 -> 'b',
		BLACK_QUEEN -> 'q', BLACK_KING -> 'k')

	// constants connected with castling
	final val freeSquaresRequiredWhiteCastleQS = Array(Cord.fromString("D1"), Cord.fromString("C1"))
	// for white to castle queenside also B1 must be empty, but may be attacked
	final val additionalFreeSquareWhiteCastleQS = Cord.fromString("B1")
	final val freeSquaresRequiredWhiteCastleKS = Array(Cord.fromString("F1"), Cord.fromString("G1"))

	final val freeSquaresRequiredBlackCastleQS = Array(Cord.fromString("D8"), Cord.fromString("C8"))
	final val additionalFreeSquareBlackCastleQS = Cord.fromString("B8")
	final val freeSquaresRequiredBlackCastleKS = Array(Cord.fromString("F8"), Cord.fromString("G8"))

	final val whiteRookQSStartPos = Cord.fromString("A1")
	final val whiteRookQSEndPos = Cord.fromString("D1")
	final val whiteRookKSStartPos = Cord.fromString("H1")
	final val whiteRookKSEndPos = Cord.fromString("F1")

	final val whiteKingStartPos = Cord.fromString("E1")
	final val whiteKingQSEndPos = Cord.fromString("C1")
	final val whiteKingKSEndPos = Cord.fromString("G1")

	final val blackRookQSStartPos = Cord.fromString("A8")
	final val blackRookQSEndPos = Cord.fromString("D8")
	final val blackRookKSStartPos = Cord.fromString("H8")
	final val blackRookKSEndPos = Cord.fromString("F8")

	final val blackKingStartPos = Cord.fromString("E8")
	final val blackKingQSEndPos = Cord.fromString("C8")
	final val blackKingKSEndPos = Cord.fromString("G8")



	final def apply(fen : String) =
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
				board.numberOfPiecesAlive += 1
				arrayPos += 1
			}
			stringIndex += 1
		}
		stringIndex += 1 // ommit space
		board.whoseMove = if (fen.charAt(stringIndex) == 'w') Piece.WHITE
						  else 								  Piece.BLACK

		stringIndex += 2 
		var castlingRights = 0
		while (fen.charAt(stringIndex) != ' ')
		{
			fen.charAt(stringIndex) match
			{
				case 'K' => castlingRights |= 1
				case 'Q' => castlingRights |= 2
				case 'k' => castlingRights |= 4
				case 'q' => castlingRights |= 8
				case '-' => castlingRights  = 0 // ommit '-'
			}
			stringIndex += 1
		}
		board.castlingRights = castlingRights
		board.movesStack.head.castlingRightsAfter = castlingRights
		stringIndex += 1 // move to next field

		// en passant field
		val enPassantFieldStart = stringIndex
		while (stringIndex < fen.size && fen.charAt(stringIndex) != ' ')
			stringIndex += 1
		val enPassantField = fen.substring(enPassantFieldStart, stringIndex)
		if (enPassantField == "-")
			board.enPassant = 0
		else
			board.enPassant = Cord.fromString(enPassantField)

		stringIndex += 1
		val halfMoveClockStart = stringIndex
		while (stringIndex < fen.size && fen.charAt(stringIndex) != ' ')
			stringIndex += 1
		board.boardHashHistoryIndex = fen.substring(halfMoveClockStart, stringIndex).toInt

		// TODO: Fullmove clock not supported

		// count scores for players
		board.updateScores	
		// calculate hash
		board.updateBoardHash
		board.boardHashHistory(board.boardHashHistoryIndex) = board.boardHash
		board
	} 
}
