package src;

class CapturePromotionMove(startPos : Int, endPos : Int, 
	castlingRightsAfterMove : Int, prom : Int)
	extends PromotionMove(startPos, endPos, castlingRightsAfterMove, prom)
{
	moveType = Move.CAPTURE_PROMOTION_MOVE
	var capturedPiece : Piece = null

	override def apply(b : Board) = 
	{
		// store captured piece
		val capturedPieceID = b.board(endPos)
		capturedPiece = b.piecesList(capturedPieceID)
		b.piecesList(capturedPieceID) = null // capture

		// promote pawn
		super.apply(b)
	}

	override def undo(b : Board) = 
	{
		// undo promotion
		super.undo(b)

		// restore captured piece
		b.piecesList(capturedPiece.id) = capturedPiece
		b.board(endPos) = capturedPiece.id
	}

	override def calculateScore(b : Board) =
		score = Piece.VALUE(b.board(end)) - Piece.VALUE(b.board(start)) + 20000

}
