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
/*
  def play(t: TicTacToe, moves: Seq[TMove]): TicTacToe = {
    var temp:TicTacToe = t
    //determining the current player
    var currentPlayer:Player = PlayerA //if the map is empty, the current player is the human player PlayerA
    if (temp.moveHistory.nonEmpty){ //if the map is not empty, the currentPlayer is the player that did not make the last move
      currentPlayer = temp.moveHistory.last._2 match {  //match the player of the last turn
        case PlayerA => PlayerB
        case PlayerB => PlayerA
      }
    }

    //simulating the moves:  (besides turn, we can also check winning etc.)
    for (move <- moves) {
      temp = temp.turn(move, currentPlayer)

      //if (currentPlayer == PlayerA) {
      //  currentPlayer == PlayerB
      //} else {
      //  currentPlayer == PlayerA
      //}

      currentPlayer = currentPlayer match {
        case PlayerB => PlayerA
        case PlayerA => PlayerB
      }
    }

    temp


  }*/


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
    if (moveHistory.size >= 9 || winner!=None) {
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


    /*
  def returnCaseObject(index:Int):TMove = {
    //allMoves.filter(_.idx == index).head
    if (index >= 0 && index < allMoves.size) {
      allMoves.find { _.idx == index }.get
    } else {
      //fucking default value: not beautiful, there should be an option that can return None for the else clause
      TopCenter
    }
  }
  */

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


    val a:Set[TMove] = Set(TopLeft, TopCenter, TopRight)
    val b:Set[TMove] = Set(MiddleLeft, MiddleCenter, MiddleRight)
    val c:Set[TMove] = Set(BottomLeft, BottomCenter, BottomRight)
    val d:Set[TMove] = Set(TopLeft, MiddleLeft, BottomLeft)
    val e:Set[TMove] = Set(TopCenter, MiddleCenter, BottomCenter)
    val f:Set[TMove] = Set(TopRight, MiddleRight, BottomRight)
    val g:Set[TMove] = Set(TopLeft, MiddleCenter, BottomRight)
    val h:Set[TMove] = Set(TopRight, MiddleCenter, BottomLeft)


    if(a.diff(moves).size == 0 || b.diff(moves).size == 0 || c.diff(moves).size == 0 ||
      d.diff(moves).size == 0 || e.diff(moves).size == 0 || f.diff(moves).size == 0 ||
      g.diff(moves).size == 0 || h.diff(moves).size == 0) {true } else {false}
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

  NMINMAX ALGORITHM USED
  src1: http://www3.ntu.edu.sg/home/ehchua/programming/java/javagame_tictactoe_ai.html
  src2: http://letstalkdata.com/2015/01/implementing-minimax-in-scala-naive-minimax/
    */

  def remainingMovesSimulated(executedMoves: Map[TMove, Player]): Set[TMove] = {
    //val allMoves = Seq[TMove](TopLeft, TopCenter,  TopRight, MiddleCenter, MiddleLeft, MiddleRight, BottomCenter,BottomLeft,BottomRight)
    //allMoves.filter(!madeMoves.contains(_)).toSet
    (allMoves filterNot executedMoves.keySet.contains).toSet
    //allMoves.diff(madeMoves).toSet


  }


  //simple algorithm that returns the TMove that would cause a win for the enemy in the shortest time:
  private def playSimulated():(TMove) = {
    val moveScores = {
      for {
        move <- remainingMoves
      } yield move -> minimax()
    }
    val bestMove = moveScores.minBy(_._2)._1  //first selecting the lowest value entry (Integer result for minimize and maximize) then returning the best move
    bestMove
  }

  private def minimax():Int = {
    //val moveHis:Map[TMove, Player] = moveHistory;
    //minimize(moveHistory,maxDepth) //maxDepth limitation is not necessary here because we have 9 turns at max. so in terms of performance this should not be a problem
    minimize(moveHistory)
  }

  //private def minimize(moveHistorySimulated:Map[TMove, Player],depth:Int):Int = {
  private def minimize(moveHistorySimulated:Map[TMove, Player]):Int = {
    val remainingMovesCurrent = remainingMovesSimulated(moveHistorySimulated)
    if(winnerSimulate(moveHistorySimulated).isDefined || remainingMovesCurrent.isEmpty) return evaluate(moveHistorySimulated)
    var bestResult = Integer.MAX_VALUE
    remainingMovesCurrent.foreach(move => {
      val bestChildResult = maximize(turnSimulated(moveHistorySimulated,move,PlayerA))  //check what player should be maximized and what minimized
      bestResult = math.min(bestResult, bestChildResult)
    })
    bestResult
  }

  //private def maximize(moveHistorySimulated:Map[TMove, Player],depth:Int):Int = {
  private def maximize(moveHistorySimulated:Map[TMove, Player]):Int = {
    val remainingMovesCurrent = remainingMovesSimulated(moveHistorySimulated)
    if(winnerSimulate(moveHistorySimulated).isDefined || remainingMovesCurrent.isEmpty) return evaluate(moveHistorySimulated)
    var bestResult = Integer.MIN_VALUE
    remainingMovesCurrent.foreach(move => {
      val bestChildResult = minimize(turnSimulated(moveHistorySimulated,move,PlayerB))
      bestResult = math.max(bestResult, bestChildResult)
    })
    bestResult
  }




  def winnerSimulate(moveHis:Map[TMove, Player]): Option[(Player, Set[TMove])] = {
    val movesPlayerA = moveHis.filter(_ == PlayerA).keySet
    val movesPlayerB = moveHis.filter(_ == PlayerB).keySet
    if (checkIfWon(movesPlayerA)) {
      Some(PlayerA, movesPlayerA)
    } else if (checkIfWon(movesPlayerB)) {
      Some(PlayerB, movesPlayerB)
    } else None

  }

  def winnerScore(moveHis:Map[TMove, Player]): Int = {
    val movesPlayerA = moveHis.filter(_ == PlayerA).keySet
    val movesPlayerB = moveHis.filter(_ == PlayerB).keySet
    if (checkIfWon(movesPlayerA)) {
      1
    } else if (checkIfWon(movesPlayerB)) {
      -1
    } else 0

  }


  /**
    * src: http://www3.ntu.edu.sg/home/ehchua/programming/java/javagame_tictactoe_ai.html
    */
  def evaluate(moveHis:Map[TMove, Player]):Int = {
    val movesPlayerA = moveHis.filter(_ == PlayerA).keySet;  //executedMoves of Player A
    val movesPlayerB = moveHis.filter(_ == PlayerB).keySet;  //executedMoves of Player B

    var scoreG = 0;

    def evaluateLine(field1Pos:Int, field2Pos:Int, field3Pos:Int):Int = {
      var score = 0;
      val cell1 = returnCaseObject(field1Pos);
      val cell2 = returnCaseObject(field1Pos);
      val cell3 = returnCaseObject(field1Pos);
      // First cell
      if (movesPlayerA.contains(cell1)) {
        score = 1
      } else if (movesPlayerB.contains(cell1)) {
        score = -1;
      }

      // Second cell
      if (movesPlayerA.contains(cell2)) {
        if (score == 1) {   // cell1 is mySeed
          score = 10;
        } else if (score == -1) {  // cell1 is oppSeed
          return 0;
        } else {  // cell1 is empty
          score = 1;
        }
      } else if (movesPlayerB.contains(cell2)) {
        if (score == -1) { // cell1 is oppSeed
          score = -10;
        } else if (score == 1) { // cell1 is mySeed
          return 0;
        } else {  // cell1 is empty
          score = -1;
        }
      }

      // Third cell
      if (movesPlayerA.contains(cell3)) {
        if (score > 0) {  // cell1 and/or cell2 is mySeed
          score *= 10;
        } else if (score < 0) {  // cell1 and/or cell2 is oppSeed
          return 0;
        } else {  // cell1 and cell2 are empty
          score = 1;
        }
      } else if (movesPlayerB.contains(cell3)) {
        if (score < 0) {  // cell1 and/or cell2 is oppSeed
          score *= 10;
        } else if (score > 1) {  // cell1 and/or cell2 is mySeed
          return 0;
        } else {  // cell1 and cell2 are empty
          score = -1;
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
    scoreG;
  }




  def turnSimulated(moveHis:Map[TMove, Player], p: TMove, player: Player):Map[TMove, Player] = {
    moveHis + (p -> player)
  }

}


