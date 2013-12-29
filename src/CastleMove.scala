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
		val rook = b.piecesList(rookID)
		val king = b.piecesList(kingID)

		b.board(rookStartPos) = Board.EMPTY_SQUARE
		b.board(kingStartPos) = Board.EMPTY_SQUARE

		b.board(rookEndPos) = rookID
		b.board(kingEndPos) = kingID

		b.scores(rook.color) -= rook.rank(b)
		b.scores(king.color) -= king.rank(b)

		rook.position = rookEndPos
		king.position = kingEndPos

		b.scores(rook.color) += rook.rank(b)
		b.scores(king.color) += king.rank(b)

		b.enPassant = 0
		b.castlingRights = castleRightsAfterMove
	}

	override def undo(b : Board) = 
	{
		val rook = b.piecesList(rookID)
		val king = b.piecesList(kingID)

		b.board(rookEndPos) = Board.EMPTY_SQUARE
		b.board(kingEndPos) = Board.EMPTY_SQUARE

		b.scores(rook.color) -= rook.rank(b)
		b.scores(king.color) -= king.rank(b)

		b.board(rookStartPos) = rookID
		b.board(kingStartPos) = kingID

		rook.position = rookStartPos
		king.position = kingStartPos

		b.scores(rook.color) += rook.rank(b)
		b.scores(king.color) += king.rank(b)
	}

	override def toString = Cord.toString(kingStartPos) + Cord.toString(kingEndPos)
}
