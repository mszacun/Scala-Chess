package src;

class CastleMove(val rookStartPos : Int, val rookEndPos : Int, val kingStartPos : Int,
	val kingEndPos : Int, castleRightsAfterMove : Int)
	extends Move(Move.CASTLE_MOVE, kingStartPos, kingEndPos, 0, castleRightsAfterMove)
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

		rook.position = rookEndPos
		king.position = kingEndPos

		b.enPassant = 0
		b.castlingRights &= castleRightsAfterMove
		castlingRightsAfter = b.castlingRights // store for restoring while undoin move
	}

	override def undo(b : Board) = 
	{
		val rook = b.piecesList(rookID)
		val king = b.piecesList(kingID)

		b.board(rookEndPos) = Board.EMPTY_SQUARE
		b.board(kingEndPos) = Board.EMPTY_SQUARE

		b.board(rookStartPos) = rookID
		b.board(kingStartPos) = kingID

		rook.position = rookStartPos
		king.position = kingStartPos
	}

	override def toString = Cord.toString(kingStartPos) + Cord.toString(kingEndPos)

	override def calculateScore(b : Board) =
		score = 5000
}
