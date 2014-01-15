package src.gui

import javax.swing.Timer
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import scala.swing._

import src.Board
import src.Cord
import src.Move
import src.AI
import src.Piece

class GUIControler
{
	// gui view displaying chessboard
	var view : GUIBoard = null
	// engine class representing chessboard
	var board : src.Board = null

	// square on board actived by previous click
	var activeSqr120 : Int = 0

	// is computer playing against iteself?
	var computerVsComputer = true

	val blackAI = new AI(Piece.WHITE)
	val whiteAI = new AI(Piece.BLACK)
	val thinkingTime = 2 * 1000

	// timer used to give time for repaint, before computer starts thinking
	val computerThinkTimer : Timer = new Timer(100, Swing.ActionListener(e => 
	{
		val dimension = view.getSize(null)
		val (score, bestPath) = 
			if (board.whoseMove == Piece.BLACK) 
				blackAI.findNextMove(board, thinkingTime)
			else
				whiteAI.findNextMove(board, thinkingTime)

		if (bestPath != Nil)
		{
			println(bestPath + " score: " + score)
			board.makeMove(bestPath.head)
			view.repaint
		}

		if (!checkForEndGame && computerVsComputer)
			computerThinkTimer.start
	}))
	computerThinkTimer.setRepeats(false)

	def checkForEndGame : Boolean = 
	{
		if (board.generateValidMovesForNextPlayer.size == 0)
		{
			if (board.isCheck(board.whoseMove)) // checkmate
			{
				if (board.whoseMove == Piece.WHITE)
					view.showEndGame(GUIControler.BLACK_WON)
				else
					view.showEndGame(GUIControler.WHITE_WON)
			}
			else
				view.showEndGame(GUIControler.DRAW) // stalemate
			return true
		}
		if (board.countRepetitions >= 3)
		{
			view.showEndGame(GUIControler.DRAW) // threefold repetition
			return true
		}
		
		return false
	}

	// function called on each click on square on chess board
	def fieldClickedEvent(sqrNumber120 : Int) = 
	{
		val dimension = view.getSize(null)
		// if we have square on which pieces is present active
		if (board.isOccupied(activeSqr120))
		{
			if (activeSqr120 != sqrNumber120)
			{
				if (!tryMakeMove(sqrNumber120) && 
					board.isOccupiedByMe(sqrNumber120, board.whoseMove))
				{
					activeSqr120 = sqrNumber120
				}
			}
		}
		else
		{
			if (board.isOccupiedByMe(sqrNumber120, board.whoseMove))
			{
				activeSqr120 = sqrNumber120
			}
		}
	}

	// tries to move active piece to clicked square, returns if it was possible
	def tryMakeMove(targetSquare120 : Int) : Boolean = 
	{
		val dimension = view.getSize(null)
		val (moves, size) = board.generateMovesForNextPlayer

		val desiredMove = moves.filter((m : Move) =>
			m != null && m.end == targetSquare120 && m.start == activeSqr120)

		if (desiredMove.size != 0)
		{
			if (board.makeMove(desiredMove.head))
			{
				view.repaint
				activeSqr120 = 0
				checkForEndGame

				// make computer move
				computerThinkTimer.start()
			}
			else
				board.undoMove

			return true
		}
		else
			return false
	}

}

object GUIControler
{
	// possible game endings
	final val BLACK_WON = 0
	final val WHITE_WON = 1
	final val DRAW = 2
}

