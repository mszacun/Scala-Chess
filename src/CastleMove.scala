package src;

class CastleMove(val rookStartPos : Int, val rookEndPos : Int, val kingStartPos : Int,
	val kingEndPos : Int, castleRightsAfterMove : Seq[Boolean])
	extends Move(Move.CASTLE_MOVE, rookStartPos, rookEndPos, 0, castleRightsAfterMove)
{
	// id of rook taking part in castling
	var rookID = 0
	var kingID = 0

	override def apply(b : Board) =
	{
		rookID = b.board(rookStartPos)
		kingID = b.board(kingStartPos)

		b.board(rookStartPos) = Board.EMPTY_SQUARE
		b.board(kingStartPos) = Board.EMPTY_SQUARE

		b.board(rookEndPos) = rookID
		b.board(kingEndPos) = kingID

		b.piecesList(rookID).position = rookEndPos
		b.piecesList(kingID).position = kingEndPos

		b.enPassant = 0
		b.castlingRights = castleRightsAfterMove
	}

	override def undo(b : Board) = 
	{
		b.board(rookEndPos) = Board.EMPTY_SQUARE
		b.board(kingEndPos) = Board.EMPTY_SQUARE

		b.board(rookStartPos) = rookID
		b.board(kingStartPos) = kingID

		b.piecesList(rookID).position = rookStartPos
		b.piecesList(kingID).position = kingStartPos
	}

	override def toString = Cord.toString(kingStartPos) + Cord.toString(kingEndPos)
}
