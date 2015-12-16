package fhj.swengb.assignments.ttt.aschneider


import scala.collection.Set

/**
  * models the different moves the game allows
  *
  * each move is made by either player a or player b.
  */
sealed trait TMove {
  def idx: Int
}

case object TopLeft extends TMove {
  override def idx: Int = 0
}

case object TopCenter extends TMove {
  override def idx: Int = 1
}

case object TopRight extends TMove {
  override def idx: Int = 2
}

case object MiddleLeft extends TMove {
  override def idx: Int = 3
}

case object MiddleCenter extends TMove {
  override def idx: Int = 4
}

case object MiddleRight extends TMove {
  override def idx: Int = 5
}

case object BottomLeft extends TMove {
  override def idx: Int = 6
}

case object BottomCenter extends TMove {
  override def idx: Int = 7
}

case object BottomRight extends TMove {
  override def idx: Int = 8
}




/**
  * for a tic tac toe game, there are two players, player A and player B
  */
sealed trait Player

case object PlayerA extends Player

case object PlayerB extends Player

object TicTacToe {

  /**
    * creates an empty tic tac toe game
    *
    * @return
    */
  def apply(): TicTacToe = new TicTacToe(Map());

  /**
    * For a given tic tac toe game, this function applies all moves to the game.
    * The first element of the sequence is also the first move.
    *
    * @param t
    * @param moves
    * @return
    */


  def play(t: TicTacToe, moves: Seq[TMove]): TicTacToe = {
    var temp:TicTacToe = t

    for (move <- moves) {
      temp = temp.turn(move, t.nextPlayer) //the current player is always the nextPlayer of the previous turn
      println("Winner: %s, \nGameOver: %s, \nRemaining moves: %s", temp.winner.get.toString(), temp.gameOver.toString(), temp.remainingMoves.toString());
    }
    temp
  }


  /**
    * creates all possible games.
    *
    * @return
    */
  def mkGames(): Map[Seq[TMove], TicTacToe] = {
    Map[Seq[TMove], TicTacToe](Seq() -> new TicTacToe(Map(), PlayerA),
      Seq(MiddleCenter) -> new TicTacToe(Map(MiddleCenter -> PlayerB), PlayerA),
      Seq(TopLeft) -> new TicTacToe(Map(TopLeft -> PlayerB), PlayerA),
      Seq(TopRight) -> new TicTacToe(Map(TopRight -> PlayerB), PlayerA),
      Seq(TopCenter) -> new TicTacToe(Map(TopCenter -> PlayerB), PlayerA),
      Seq(MiddleLeft) -> new TicTacToe(Map(MiddleLeft -> PlayerB), PlayerA),
      Seq(MiddleRight) -> new TicTacToe(Map(MiddleRight -> PlayerB), PlayerA),
      Seq(BottomCenter) -> new TicTacToe(Map(BottomCenter -> PlayerB), PlayerA),
      Seq(BottomLeft) -> new TicTacToe(Map(BottomLeft -> PlayerB), PlayerA),
      Seq(BottomRight) -> new TicTacToe(Map(BottomRight -> PlayerB), PlayerA)
    )

  }

}

/**
  * Models the well known tic tac toe game.
  *
  * The map holds the information which player controls which field.
  *
  * The nextplayer parameter defines which player makes the next move.
  */
case class TicTacToe(moveHistory: Map[TMove, Player],
                     nextPlayer: Player = PlayerA) {

  val madeMoves = Seq[TMove]()
  val allMoves = Seq[TMove](TopLeft, TopCenter,  TopRight,MiddleLeft, MiddleCenter, MiddleRight,BottomLeft, BottomCenter,BottomRight)

  /**
    * outputs a representation of the tic tac toe like this:
    *
    * |---|---|---|
    * | x | o | x |
    * |---|---|---|
    * | o | x | x |
    * |---|---|---|
    * | x | o | o |
    * |---|---|---|
    *
    * @return
    */

  def asString(): String = {
    var board: String =
        "|---|---|---|\n" +
        "|   |   |   |\n" +
        "|---|---|---|\n" +
        "|   |   |   |\n" +
        "|---|---|---|\n" +
        "|   |   |   |\n" +
        "|---|---|---|\n\n"

    val boardMap = Map( 0 -> 16, 1 -> 20, 2 -> 24,
      3 -> 44, 4 -> 48, 5 -> 52,
      6 -> 72, 7 -> 76, 8 -> 80)

    for ((x, p) <- moveHistory) {
      if (p == PlayerA) {
        board = board.updated(boardMap(x.idx), "o").mkString
      }
      else if (p == PlayerB) {
        board = board.updated(boardMap(x.idx), "x").mkString
      }
      else {
        board = board.updated(boardMap(x.idx), " ").mkString
      }
    }
    board

  }

  /**
    * is true if the game is over.
    *
    * The game is over if either of a player wins or there is a draw.
    */
  val gameOver: Boolean = {
    if (moveHistory.size >= 9 || winner.isDefined) {
      true
    } else {
      false
    }

  }


  /**
    * the moves which are still to be played on this tic tac toe.
    */
  val remainingMoves: Set[TMove] = {

    allMoves.filter(!moveHistory.keySet.contains(_)).toSet

  }

  def returnCaseObject(index:Int):TMove = {
    allMoves.find(_.idx == index).get //find returns an option, so we get the value out of the option
  }

  def returnCaseObjectMove(index:Int):TMove = {
    allMoves.filter(_.idx == index).head
  }

  /**
    * given a tic tac toe game, this function returns all
    * games which can be derived by making the next turn. that means one of the
    * possible turns is taken and added to the set.
    */
  lazy val nextGames: Set[TicTacToe] = nextGames.+(TicTacToe(moveHistory: Map[TMove, Player]))


  def checkIfWon(moves:Set[TMove]):Boolean = {

    val topRow:Set[TMove] = Set(TopLeft, TopCenter, TopRight)
    val centerRow:Set[TMove] = Set(MiddleLeft, MiddleCenter, MiddleRight)
    val bottomRow:Set[TMove] = Set(BottomLeft, BottomCenter, BottomRight)
    val leftColumn:Set[TMove] = Set(TopLeft, MiddleLeft, BottomLeft)
    val middleColumn:Set[TMove] = Set(TopCenter, MiddleCenter, BottomCenter)
    val rightColumn:Set[TMove] = Set(TopRight, MiddleRight, BottomRight)
    val diagonalLeftToRight:Set[TMove] = Set(TopLeft, MiddleCenter, BottomRight)
    val diagonalRightToLeft:Set[TMove] = Set(TopRight, MiddleCenter, BottomLeft)


    if(topRow.diff(moves).isEmpty || centerRow.diff(moves).isEmpty|| bottomRow.diff(moves).isEmpty ||
      leftColumn.diff(moves).isEmpty || middleColumn.diff(moves).isEmpty || rightColumn.diff(moves).isEmpty ||
      diagonalLeftToRight.diff(moves).isEmpty || diagonalRightToLeft.diff(moves).isEmpty) {
      true
    } else {
      false
    }
  }


  /**
    * Either there is no winner, or PlayerA or PlayerB won the game.
    *
    * The set of moves contains all moves which contributed to the result.
    */
  def winner: Option[(Player, Set[TMove])] = {
    val movesPlayerA = moveHistory.filter(_._2 == PlayerA).keySet
    val movesPlayerB = moveHistory.filter(_._2 == PlayerB).keySet


    if (checkIfWon(movesPlayerA)) {
      Some(PlayerA, movesPlayerA)
    } else if (checkIfWon(movesPlayerB)) {
      Some(PlayerB, movesPlayerB)
    } else None

  }



  /**
    * returns a copy of the current game, but with the move applied to the tic tac toe game.
    *
    * @param move   to be played
    * @param player the player
    * @return
    */
  def turn(move: TMove, player: Player): TicTacToe = {

    if (!moveHistory.keySet.contains(move)) {
      if (player == PlayerA) {
        TicTacToe(moveHistory + (move -> player), PlayerB)
      } else {
        TicTacToe(moveHistory + (move -> player), PlayerA)
      }

    }
    else {
      TicTacToe(moveHistory, player)
    }

  }



  /**

  CPU PLAYER CODE

  NMINMAX ALGORITHM USED:
  src1: http://www3.ntu.edu.sg/home/ehchua/programming/java/javagame_tictactoe_ai.html
  src2: http://letstalkdata.com/2015/01/implementing-minimax-in-scala-naive-minimax/
    */

  def remainingMovesSimulated(executedMoves: Map[TMove, Player]): Set[TMove] = {
    (allMoves filterNot executedMoves.keySet.contains).toSet
  }


  val enemyPlayer:Player = PlayerA;
  val humanPlayer:Player = PlayerB;


  def makeMove():(TMove) = {
    val currentPlayer:Player = nextPlayer match {
      case PlayerA => PlayerB;
      case PlayerB => PlayerA;
    }

    minimax3(moveHistory,currentPlayer,nextPlayer,9,Integer.MIN_VALUE, Integer.MAX_VALUE)._1;
  }


  /** Recursive minimax at level of depth for either maximizing or minimizing player.
       Return int[3] of {score, row, col}  */
  private def minimax3(moveHistorySimulated:Map[TMove, Player],player:Player, nexPlayer:Player,depth:Int, alpha:Int, beta: Int):Tuple2[TMove, Int] = {
    // Generate possible next moves in a List of int[2] of {row, col}.
    val nextMoves = remainingMovesSimulated(moveHistorySimulated)
    var alphaNew = alpha
    var betaNew = beta
    //var bestScore:Int = if(player == enemyPlayer) {Integer.MIN_VALUE} else {Integer.MAX_VALUE}
    // mySeed is maximizing; while oppSeed is minimizing
    var currentScore:Int = 0
    var bestMove:TMove = MiddleCenter

    if(winnerSimulate(moveHistorySimulated).isDefined || nextMoves.isEmpty || depth == 0) {
      // Gameover or depth reached, evaluate score
      currentScore = evaluate(moveHistorySimulated, enemyPlayer, humanPlayer)
    } else {

      for (move <- nextMoves) {
        if (alphaNew < betaNew) {
          // Try this move for the current "player"
          if (player == enemyPlayer) {  // mySeed (computer) is maximizing player
            currentScore = minimax3(moveHistorySimulated + (move -> player),humanPlayer, enemyPlayer, depth - 1, alphaNew, betaNew)._2;
            if (currentScore > alphaNew) {
              alphaNew = currentScore
              bestMove = move
            }
          } else {  // oppSeed is minimizing player
            currentScore = minimax3(moveHistorySimulated + (move -> player),enemyPlayer, humanPlayer, depth - 1, alphaNew, betaNew)._2;
            if (currentScore < betaNew) {
              betaNew = currentScore
              bestMove = move
            }
          }

        }
      }
    }
    new Tuple2[TMove, Int](bestMove, currentScore)
  }

  //simple algorithm that returns the TMove that would cause a win for the enemy in the shortest time:
  def playSimulated():(TMove) = {
    val currentPlayer:Player = nextPlayer match {
      case PlayerA => PlayerB;
      case PlayerB => PlayerA;
    }

    val moveScores = {
      for {
        move <- remainingMoves
      } yield move -> minimax2(currentPlayer, nextPlayer)//minimax(currentPlayer, nextPlayer)
    }
    val bestMove = moveScores.maxBy(_._2)._1  //first selecting the lowest value entry (Integer result for minimize and maximize) then returning the best move
    bestMove
  }


  private def minimax2(curPlayer: Player, nexPlayer: Player):Int = {
    val maxDepth = 3;

    def minimize(moveHistorySimulated:Map[TMove, Player], depth:Int, alpha:Int, beta:Int):Int = {
      val remainingMovesCurrent = remainingMovesSimulated(moveHistorySimulated)
      if(winnerSimulate(moveHistorySimulated).isDefined || remainingMovesCurrent.isEmpty || depth == 0) return evaluate(moveHistorySimulated, enemyPlayer, humanPlayer)

      var newBeta = beta
      remainingMovesCurrent.foreach(move => {
        newBeta = math.min(beta, maximize(turnSimulated(moveHistorySimulated,move,curPlayer), depth - 1, alpha, newBeta))
        if (alpha >= newBeta) return alpha
      })
      newBeta
    }

    def maximize(moveHistorySimulated:Map[TMove, Player], depth:Int, alpha:Int, beta:Int):Int = {
      val remainingMovesCurrent = remainingMovesSimulated(moveHistorySimulated)
      if(winnerSimulate(moveHistorySimulated).isDefined || remainingMovesCurrent.isEmpty || depth == 0) return evaluate(moveHistorySimulated, enemyPlayer, humanPlayer)

      var newAlpha = alpha
      remainingMovesCurrent.foreach(move => {
        newAlpha = math.max(beta, maximize(turnSimulated(moveHistorySimulated,move,nexPlayer), depth - 1, newAlpha, beta))
        if (newAlpha >= beta) return beta
      })
      newAlpha
    }

    minimize(moveHistory, maxDepth, Integer.MIN_VALUE, Integer.MAX_VALUE)
  }



  private def minimax(curPlayer: Player, nexPlayer: Player):Int = {
    val maxDepth = 3
     def minimize(moveHistorySimulated:Map[TMove, Player], depth:Int):Int = {
      val remainingMovesCurrent = remainingMovesSimulated(moveHistorySimulated)
       if(winnerSimulate(moveHistorySimulated).isDefined || remainingMovesCurrent.isEmpty || depth == 0) return evaluate(moveHistorySimulated, enemyPlayer, humanPlayer)
      var bestResult = Integer.MAX_VALUE
      for (move <- remainingMovesCurrent) {
        val bestChildResult = maximize(turnSimulated(moveHistorySimulated,move,curPlayer), depth - 1)  //check what player should be maximized and what minimized
        bestResult = math.min(bestResult, bestChildResult)
      }
        bestResult
    }

   def maximize(moveHistorySimulated:Map[TMove, Player], depth:Int):Int = {
      val remainingMovesCurrent = remainingMovesSimulated(moveHistorySimulated)
    if(winnerSimulate(moveHistorySimulated).isDefined || remainingMovesCurrent.isEmpty || depth == 0) return evaluate(moveHistorySimulated, enemyPlayer, humanPlayer)
      var bestResult = Integer.MIN_VALUE
      for (move <- remainingMovesCurrent) {
        val bestChildResult = minimize(turnSimulated(moveHistorySimulated,move,nexPlayer), depth - 1)
        bestResult = math.max(bestResult, bestChildResult)
      }

      bestResult
    }


    minimize(moveHistory, maxDepth)
  }


  def winnerSimulate(moveHis:Map[TMove, Player]): Option[(Player, Set[TMove])] = {
    val movesPlayerA = moveHis.filter(_._2 == PlayerA).keySet
    val movesPlayerB = moveHis.filter(_._2 == PlayerB).keySet
    if (checkIfWon(movesPlayerA)) {
      Some(PlayerA, movesPlayerA)
    } else if (checkIfWon(movesPlayerB)) {
      Some(PlayerB, movesPlayerB)
    } else None

  }

  def winnerScore(moveHis:Map[TMove, Player]): Int = {
    val movesPlayerA = moveHis.filter(_._2 == PlayerA).keySet
    val movesPlayerB = moveHis.filter(_._2 == PlayerB).keySet
    if (checkIfWon(movesPlayerA)) {
      1
    } else if (checkIfWon(movesPlayerB)) {
      -1
    } else 0

  }


  /**
    * src: http://www3.ntu.edu.sg/home/ehchua/programming/java/javagame_tictactoe_ai.html
    */
  //playerCurrent should be the computer, playerNext should be human
  def evaluate(moveHis:Map[TMove, Player], playerCurrent:Player, playerNext:Player):Int = {
    val movesPlayerA = moveHis.filter(_._2 == playerCurrent).keySet;  //executedMoves of Player A
    val movesPlayerB = moveHis.filter(_._2 == playerNext).keySet;  //executedMoves of Player B
    //println("Moves of the current player: " + movesPlayerA.map(_.idx.toString()));
   // println("Moves of the next player: " + movesPlayerB.map(_.idx.toString()));

    var scoreG = 0

    def evaluateLine(field1Pos:Int, field2Pos:Int, field3Pos:Int):Int = {
      var score = 0
      val cell1 = returnCaseObject(field1Pos)
      val cell2 = returnCaseObject(field2Pos)
      val cell3 = returnCaseObject(field3Pos)
      // First cell
      if (movesPlayerA.contains(cell1)) {
        score = 1
      } else if (movesPlayerB.contains(cell1)) {
        score = -1
      }

      // Second cell
      if (movesPlayerA.contains(cell2)) {
        if (score == 1) {   // cell1 is mySeed
          score = 10
        } else if (score == -1) {  // cell1 is oppSeed
          return 0
        } else {  // cell1 is empty
          score = 1
        }
      } else if (movesPlayerB.contains(cell2)) {
        if (score == -1) { // cell1 is oppSeed
          score = -10
        } else if (score == 1) { // cell1 is mySeed
          return 0
        } else {  // cell1 is empty
          score = -1
        }
      }

      // Third cell
      if (movesPlayerA.contains(cell3)) {
        if (score > 0) {  // cell1 and/or cell2 is mySeed
          score *= 10
        } else if (score < 0) {  // cell1 and/or cell2 is oppSeed
          //function should exit here!
          return 0
        } else {  // cell1 and cell2 are empty
          score = 1
        }
      } else if (movesPlayerB.contains(cell3)) {
        if (score < 0) {  // cell1 and/or cell2 is oppSeed
          score *= 10
        } else if (score > 1) {  // cell1 and/or cell2 is mySeed
          return 0
        } else {  // cell1 and cell2 are empty
          score = -1
        }
      }
      score
    }

    scoreG += evaluateLine(0,1,2);  // row 0
    scoreG += evaluateLine(3,4,5);  // row 1
    scoreG += evaluateLine(6,7,8);  // row 2
    scoreG += evaluateLine(0,3,6);  // col 1
    scoreG += evaluateLine(1,4,7);  // col 2
    scoreG += evaluateLine(2,5,8);  // col 3
    scoreG += evaluateLine(0,4,8);  // diagonal
    scoreG += evaluateLine(2,4,6);  // alternate diagonal
    scoreG
  }


  def turnSimulated(moveHis:Map[TMove, Player], p: TMove, player: Player):Map[TMove, Player] = {
    moveHis + (p -> player)
  }

}


