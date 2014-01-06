package src

import scala.util.Random

// WARNING: Because of lack of unisgned number types iin JVM, we have to
// shift hashcode left, to avoid negative numbers, so we effectively use only 
// 63 bits of hashcode =[

class TranspositionTable(val size : Int)
{
	class TranspositionTableEntry(val key : Long, val move : List[Move], val score : Int,
		val depth : Int, val nodeType : Int)
	{
	}

	final val table = new Array[TranspositionTableEntry](size)

	final def getMove(key : Long) : Move =
	{
		val index : Int = ((key >>> 1) % size).toInt
		if (table(index) != null && table(index).key == key && 
			table(index).move != Nil)
			return table(index).move.head
		else
			return null
	}

	final def getScore(key : Long, depth : Int, alpha : Int, beta : Int) : Int =
	{
		val index : Int = ((key >>> 1) % size).toInt
		val entry = table(index)

		if (entry != null && entry.key == key && entry.depth >= depth)
		{
			if (entry.nodeType == Hash.EXACT)
				return entry.score
			if (entry.nodeType == Hash.FAIL_LOW_ALPHA && entry.score <= alpha)
				return alpha
			if (entry.nodeType == Hash.FAIL_HIGH_ALPHA && entry.score >= beta)
				return beta
			if (entry.nodeType == Hash.FAIL_LOW_BETA && entry.score <= alpha)
				return alpha
			if (entry.nodeType == Hash.FAIL_HIGH_BETA && entry.score >= beta)
				return beta
		}
		// information in table is unusable

		return Hash.UNKNOW_VALUE
	}

	final def set(key : Long, move : List[Move], score : Int, depth : Int,
		nodeType : Int) : Unit =
	{
		val index : Int = ((key >>> 1) % size).toInt
		val previous = table(index)

		// store only if at least the same depth
		if (previous == null || previous.depth <= depth)
			table(index) = new TranspositionTableEntry(key, move, score, depth,
				nodeType)
	}
}

	

object Hash
{
	// piecesHash[pieceID][position]
	final val piecesHash = new Array[Array[Long]](32)

	// castleRightsHash[castleRights]
	final val castleRightsHash = new Array[Long](16)

	final var whiteMovesHash : Long = 0

	final val enPassantSquareHash = new Array[Long](65)

	final def initHashTables = 
	{
		val r = new Random

		/* piece hashes */
		for (i <- 0 until 32)
		{
			piecesHash(i) = new Array[Long](65)
			for (j <- 0 until 65)
				piecesHash(i)(j) = r.nextLong
		}

		/* castle right hashes */
		for (i <- 0 until 16)
			castleRightsHash(i) = r.nextLong

		whiteMovesHash = r.nextLong

		for (i <- 0 until 65)
			enPassantSquareHash(i) = r.nextLong
	}

	final val EXACT = 0 // exact score for node
	final val FAIL_LOW_ALPHA = 1 // score to low to raise alpha
	final val FAIL_HIGH_ALPHA = 2 // score above beta
	final val FAIL_LOW_BETA = 3 // score below alpha
	final val FAIL_HIGH_BETA = 4 // score to high to update beta

	final val UNKNOW_VALUE = Integer.MAX_VALUE - 100

}	 
