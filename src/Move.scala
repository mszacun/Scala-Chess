package src;

abstract class Move(var moveType : Int, val start : Int, val end : Int,
	val enPassant : Int, val castlingRightsMask : Int)
{
	// * start -> position from move started (if more than one piece takes part
	// in move, primary piece start position is stored
	// * end -> position where move ends
	// * enPassant -> describes field from which en passant is possible, if
	// pawn is present on this field, after this move was made
	// * castlingRightsMask -> mask that should be anded with current catling rights
	// to get castling rights after this move
	var score = 0 // used to order moves in search function
	var castlingRightsAfter = 0 // castling rights after this move
	
	// applays move to board, doesn't check if it is possible, it should be 
 	// checked before calling this method
	def apply(b : Board) : Unit

	// undo move
	// WARNING: doesnt't restor castiling rights and en passant information,
	// that was before move was made, Board class is responsible for this,
	// because it has required informations
	def undo(b : Board) : Unit

	// calculates score of this move and stores it in 'score' field	
	def calculateScore(b : Board) : Unit

	override def equals(other : Any) = 
	{
		val o = other.asInstanceOf[Move]
		o != null && o.moveType == moveType && o.start == start && o.end == end
	}

	override def toString = Cord.toString(start) + Cord.toString(end)

}

object Move
{
	val QUIET_MOVE = 0
	val CAPTURE_MOVE = 1
	val CASTLE_MOVE = 2
	val PROMOTION_MOVE = 3
	val ENPASSANT_MOVE = 4
	val CAPTURE_PROMOTION_MOVE = 5
}
