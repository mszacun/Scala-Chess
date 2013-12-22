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

	def ifOccupied(position : Int) = board(position) > 0

	def isOffTheBoard(position : Int) = board(position) == Board.AUXILIARY_SQUARE
}	

object Board
{
	val AUXILIARY_SQUARE = -1
	val EMPTY_SQUARE = 0
	// keys in piece list
	val WHITE_PAWN_1 = 1
	val WHITE_PAWN_2 = 2
	val WHITE_PAWN_3 = 3
	val WHITE_PAWN_4 = 4
	val WHITE_PAWN_5 = 5
	val WHITE_PAWN_6 = 6
	val WHITE_PAWN_7 = 7
	val WHITE_PAWN_8 = 8
	
	val WHITE_ROOK_1 = 9
	val WHITE_ROOK_2 = 10
	val WHITE_KNIGHT_1 = 11
	val WHITE_KNIGHT_2 = 12
	val WHITE_BISHOP_1 = 13
	val WHITE_BISHOP_2 = 14
	val WHITE_QUEEN = 15
	val WHITE_KING = 16

	
	val BLACK_PAWN_1 = 17
	val BLACK_PAWN_2 = 18
	val BLACK_PAWN_3 = 19
	val BLACK_PAWN_4 = 20
	val BLACK_PAWN_5 = 21
	val BLACK_PAWN_6 = 22
	val BLACK_PAWN_7 = 23
	val BLACK_PAWN_8 = 24
	
	val BLACK_ROOK_1 = 25
	val BLACK_ROOK_2 = 26
	val BLACK_KNIGHT_1 = 27
	val BLACK_KNIGHT_2 = 28
	val BLACK_BISHOP_1 = 29
	val BLACK_BISHOP_2 = 30
	val BLACK_QUEEN = 31
	val BLACK_KING = 32
}
