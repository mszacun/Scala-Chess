package src;

class CaptureMove(start : Int, end : Int, 
	castlingRightsAfterMove : Int) extends Move(Move.CAPTURE_MOVE,
	start, end, 0, castlingRightsAfterMove)
	{
		// will be assigned during applaying to board
		var capturedPiece : Piece = null 
		var capturedPieceID : Int = -1
		var previousLastUndoableMove = 0

		override def apply(b : Board) =
		{
			val pieceID = b.board(start)
			val capturingPiece = b.piecesList(pieceID)
			capturedPieceID = b.board(end)
			capturedPiece = b.piecesList(capturedPieceID)

			b.board(end) = pieceID
			b.board(start) = Board.EMPTY_SQUARE
			
			capturingPiece.position = end
			b.piecesList(capturedPieceID) = null

			b.castlingRights &= castlingRightsMask
			castlingRightsAfter = b.castlingRights // store for restoring while undoin move
			b.enPassant = 0
			b.numberOfPiecesAlive -= 1

			previousLastUndoableMove = b.lastUndoAbleMove
			b.lastUndoAbleMove = b.boardHashHistoryIndex
		}

		override def undo(b : Board) = 
		{
			val pieceID = b.board(end)
			val capturingPiece = b.piecesList(pieceID)

			b.board(start) = pieceID
			b.board(end) = capturedPieceID

			capturingPiece.position = start
			b.piecesList(capturedPieceID) = capturedPiece
			b.numberOfPiecesAlive += 1

			b.lastUndoAbleMove = previousLastUndoableMove
		}

		override def calculateScore(b : Board) = 
			score = Piece.VALUE(b.board(end)) - Piece.VALUE(b.board(start)) + 10000


	}

