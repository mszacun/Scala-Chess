package src;

abstract class Piece(var position : Int, val color : Boolean, val id : Int)
{
	// * position -> actual position on board as index in array
	// reprezenting board
	// * color -> true - white, false - black
	// * id -> id of this particular piece in board piece list, see Board class

	// constructs piece from human-readable field name
	def this(position : String, color : Boolean, id : Int) = 
		this(Cord.fromString(position), color, id)

	// WARNING: All generating moves methods may return moves which destination is
	// outside the board! Board class is responsible for checking this, also
	// generated moves may case own king to be in check, Board class also has to
	// take care of it

	// moves, that dont causes capture
	def generateQuietMoves(b : Board) : Traversable[Move]

	// possible captures this piece can make
	def generateAttacks(b : Board) : Traversable[Move]

	// quiteMoves + attacks
	def generateMoves(b : Board) : Traversable[Move]

	// rank value of this piece in actual position
	// takes under consideration material value of piece and
	// actual position of piece on board
	def rank : Int
}
