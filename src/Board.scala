package src;

class Board(fen : String = "") 
{
	// TODO: Implement fen parsing

	val board : Array[Int] = new Array[Int](10 * 12)
	val piecesList : Array[Piece] = new Array[Piece](32)
	var castlingRights : Seq[Boolean] = Array(true, true, true, true)
	var enPassant1 : Int = 0
	var enPassant2 : Int = 0

	/* empty board */
	for	(i <- 0 to 119)
	{
		if (i < 20 || i > 100 || i % 10 == 0 || i % 10 == 9)
			board(i) = Board.AUXILIARY_SQUARE
		else
			board(i) = Board.EMPTY_SQUARE
	}

	/* TODO: Game history: here or in Game class */

	def addPiece(piece : Piece) = 
	{
		board(piece.position) = piece.id
		piecesList(piece.id) = piece
	}

	// WARNING: if you use one of those methods, make sure, you've choosen the
	// right one, becasue there are no 2 like in boolean, but 3(empty, occupied, 
	// off the board)!!

	def isEmpty(position : Int) = board(position) == Board.EMPTY_SQUARE

	def isOccupied(position : Int) = board(position) > 0

	// quickly checks color of piece
	def isOccupiedByOpponent(position : Int, myColor : Int) = 
		isOccupied(position) && (board(position) % 2) != myColor

	def isOffTheBoard(position : Int) = board(position) == Board.AUXILIARY_SQUARE
}	

object Board
{
	val AUXILIARY_SQUARE = -1
	val EMPTY_SQUARE = 0
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
	val BLACK_PAWN_1 = 2
	val BLACK_PAWN_2 = 4
	val BLACK_PAWN_3 = 6
	val BLACK_PAWN_4 = 8
	val BLACK_PAWN_5 = 10
	val BLACK_PAWN_6 = 12
	val BLACK_PAWN_7 = 14
	val BLACK_PAWN_8 = 16
	
	val BLACK_ROOK_1 = 18
	val BLACK_ROOK_2 = 20
	val BLACK_KNIGHT_1 = 22
	val BLACK_KNIGHT_2 = 24
	val BLACK_BISHOP_1 = 26
	val BLACK_BISHOP_2 = 28
	val BLACK_QUEEN = 30
	val BLACK_KING = 32
}
