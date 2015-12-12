package fhj.swengb.assignments.ttt.aschneider

import javafx.application.Application
import javafx.scene.shape.{CubicCurveTo, MoveTo, Path}
import javafx.stage.Stage
import java.awt.{TextField, Button}
import java.beans.EventHandler
import java.net.URL
import java.rmi.activation.ActivationGroup_Stub
import java.util.ResourceBundle
import javafx.animation._
import javafx.application.Application
import javafx.fxml.{FXML, FXMLLoader, Initializable}
import javafx.scene._
import javafx.scene.control.Label
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout.{AnchorPane, GridPane, BorderPane}
import javafx.stage.Stage
import javafx.util.Duration
import com.sun.javaws.jnl.JavaFXAppDesc
import com.sun.org.apache.xpath.internal.functions.FuncFalse
import scala.util.control.NonFatal


//case class highscore() {
//  val iwas : String
//}


//object launches the class described beneath
object TicTacToeApp {
  def main(args: Array[String]) {
    Application.launch(classOf[TicTacToeApp], args: _*)
  }
}


class TicTacToeApp extends javafx.application.Application {


  val Css = "/fhj/swengb/assignments/ttt/aschneider/TicTacToe.css"
  val Fxml = "/fhj/swengb/assignments/ttt/aschneider/TicTacToeApp.fxml"


  val loader = new FXMLLoader(getClass.getResource(Fxml))

  override def start(stage: Stage): Unit =
    try {
      stage.setTitle("TicTacJoe")
      loader.load[Parent]() // side effect
      val scene = new Scene(loader.getRoot[Parent]) //loads the default scene
      stage.setScene(scene)
      stage.setResizable(false) //window cannot be rescaled
      //stage.getScene.getStylesheets.add(Css)
      stage.show()
    } catch {
      case NonFatal(e) => e.printStackTrace()
    }
}

//controller contains the description of the functionality of the application
class TicTacToeAppController extends TicTacToeApp {
  //attributes are being initialized (everything with an ID)
  @FXML var menu: AnchorPane = _
  //Main Pane
  @FXML var mpMenu: AnchorPane = _
  //Settings Pane for Muliplayer
  @FXML var spMenu: AnchorPane = _
  //Settings Pane for SinglePlayer
  @FXML var gamePane: AnchorPane = _
  //game Pane where the game is played (both mp and sp)
  @FXML var mpAvatar: ImageView = _
  //
  @FXML var spAvatar: ImageView = _

  @FXML var mpName1: control.TextField = _
  @FXML var mpName2: control.TextField = _
  @FXML var spName: control.TextField = _

  @FXML var playground: AnchorPane = _

  @FXML var status: control.Label = _
  @FXML var headline: control.Label = _
  @FXML var winPane: AnchorPane = _
  @FXML var winStatus: control.Label = _

  @FXML var topLeft: control.Button = _
  @FXML var topCentre: control.Button = _
  @FXML var topRight: control.Button = _
  @FXML var centreLeft: control.Button = _
  @FXML var centreCentre: control.Button = _
  @FXML var centreRight: control.Button = _
  @FXML var bottomLeft: control.Button = _
  @FXML var bottomCentre: control.Button = _
  @FXML var bottomRight: control.Button = _


  var playingInSpMode: Boolean = true
  //
  var player1Playing: Boolean = false

  /*
  Positions:  0 | 1 | 2
              ---------
              3 | 4 | 5
              ---------
              6 | 7 | 8
   */

  var board = new Array[Char](9)
  var counter = 1;


  //initialize function executes the commands at startup for the main scene

  def anim(obj: AnchorPane, slideRight: Boolean, xMitte: Int = 300, yMitte: Int = 400): Unit = {

    var xEnde = 1200
    var path: Path = new Path()

    if (slideRight) {
      path.getElements.add(new MoveTo(xMitte, yMitte))
      path.getElements().add(new CubicCurveTo(xMitte + 50, yMitte, xMitte + 200, yMitte, xMitte + xEnde, yMitte))
    } else {
      path.getElements.add(new MoveTo(xMitte + xEnde, yMitte))
      path.getElements().add(new CubicCurveTo(xMitte + 200, yMitte, xMitte + 50, yMitte, xMitte, yMitte))
    }

    var pathTrans: PathTransition = new PathTransition()
    pathTrans.setDuration(new Duration(200))
    pathTrans.setNode(obj)
    pathTrans.setPath(path)
    pathTrans.setAutoReverse(false)
    pathTrans.play()
  }




  def startMultiPlayer(playerName1: String, playerName2: String): Boolean = {

    var player1IsStarting: Boolean = false
    if (scala.util.Random.nextInt(100) >= 50) {
      player1IsStarting = true
      status.setText("It's " + playerName1 + "'s turn:")
    }
    else {
      player1IsStarting = false
      status.setText("It's " + playerName2 + "'s turn:")
    }
    anim(mpMenu, true, 300)
    anim(gamePane, false, 356)

    if (player1IsStarting)
      return true
    else
      return false
  }



  def startSinglePlayer(playerName: String): Boolean = {

    var playerIsStarting: Boolean = false
    if (scala.util.Random.nextInt(100) >= 50) {
      playerIsStarting = true
      status.setText("It's " + playerName + "'s turn:")
    }
    else {
      playerIsStarting = false
      status.setText("It's " + "computers" + "'s turn:")
    }

    anim(spMenu, true, 300)
    anim(gamePane, false, 356)
    if (playerIsStarting)
      return true
    else
      return false
  }




  def drawPlayGround(playerSymbol: Char, markedPos: Int): Unit = {
    if (markedPos == 0) {
      topLeft.setText(playerSymbol.toString)
      topLeft.setDisable(true)
    }
    else if (markedPos == 1) {
      topCentre.setText(playerSymbol.toString)
      topCentre.setDisable(true)
    }
    else if (markedPos == 2) {
      topRight.setText(playerSymbol.toString)
      topRight.setDisable(true)
    }
    else if (markedPos == 3) {
      centreLeft.setText(playerSymbol.toString)
      centreLeft.setDisable(true)
    }
    else if (markedPos == 4) {
      centreCentre.setText(playerSymbol.toString)
      centreCentre.setDisable(true)
    }
    else if (markedPos == 5) {
      centreRight.setText(playerSymbol.toString)
      centreRight.setDisable(true)
    }
    else if (markedPos == 6) {
      bottomLeft.setText(playerSymbol.toString)
      bottomLeft.setDisable(true)
    }
    else if (markedPos == 7) {
      bottomCentre.setText(playerSymbol.toString)
      bottomCentre.setDisable(true)
    }
    else {
      bottomRight.setText(playerSymbol.toString)
      bottomRight.setDisable(true)
    }
  }


  def checkForWinner(board: Array[Char], symbol: Char): Boolean = {

    val winBoard =
      List(
        List(0, 1, 2),
        List(3, 4, 5),
        List(6, 7, 8),
        List(0, 3, 6),
        List(1, 4, 7),
        List(2, 5, 8),
        List(0, 4, 8),
        List(2, 4, 6))

    print("\ncounter: " + counter) //== symbol)

    return winBoard.exists(winSet => winSet.forall(board(_) == symbol))

  }


  /*
      player: current playing player (is Player 1 playing?)
      gameMode: playing in Sp-Mode?
      markedPos: position played
      board: last state of the board
   */
  def mainGame(player: Boolean, computerEnemy: Boolean, markedPos: Int, board: Array[Char]): Unit = {

    val symbol1 = 'X'
    val symbol2 = 'O'
    var symbol = '?'
    if (player) symbol = symbol1
    else symbol = symbol2

    board(markedPos) = symbol

    status.setText("Drawing...")
    drawPlayGround(symbol, markedPos)

    if (checkForWinner(board, symbol)) {
      print("WE HAVE A WINNER\n")

      if (player) winStatus.setText(mpName1.getCharacters.toString + " won the game in " + counter + " steps!")
      else winStatus.setText(mpName2.getCharacters.toString + " won the game in " + counter + " steps!")

      playground.setDisable(true)
      anim(winPane, false, 370, 130)

    }


    if (computerEnemy) {
      status.setText("Computer is thinking...")
      if (symbol == symbol1) symbol = symbol2 else symbol = symbol2
      /*do{

      }while(board())*/
      //drawPlayGround(symbol,markedPos)


    }
    else {
      if (!player) status.setText("It's " + mpName1.getCharacters.toString + "'s (" + symbol1 + ") turn:")
      else status.setText("It's " + mpName2.getCharacters.toString + "'s (" + symbol2 + ") turn:")


      player1Playing = !player
      counter += 1
    }
  }


  def mpMenuBack(): Unit = anim(mpMenu, true)

  //Hier die richtigen User Infos übergeben! -> aus tabelle oda ka wie ihr sie gespeichert habts
  def spMenuBack(): Unit = anim(spMenu, true)

  def mpStartMenu(): Unit = anim(mpMenu, false);

  def spStartMenu(): Unit = anim(spMenu, false)

  def mpStart(): Unit = {
    playingInSpMode = false; player1Playing = startMultiPlayer(mpName1.getCharacters.toString, mpName2.getCharacters.toString)
  }

  def spStart(): Unit = {
    playingInSpMode = true; player1Playing = startSinglePlayer(spName.getCharacters.toString)
  }

  def backToMainMenu(): Unit = ???

  def restart(): Unit = {
    menu.getScene().getWindow().hide(); start(new Stage); counter = 0
  }


  def playTopLeft(): Unit = {
    mainGame(player1Playing, playingInSpMode, 0, board)
  }

  def playTopCentre(): Unit = {
    mainGame(player1Playing, playingInSpMode, 1, board)
  }

  def playTopRight(): Unit = {
    mainGame(player1Playing, playingInSpMode, 2, board)
  }

  def playCentreLeft(): Unit = {
    mainGame(player1Playing, playingInSpMode, 3, board)
  }

  def playCentreCentre(): Unit = {
    mainGame(player1Playing, playingInSpMode, 4, board)
  }

  def playCentreRight(): Unit = {
    mainGame(player1Playing, playingInSpMode, 5, board)
  }

  def playBottomLeft(): Unit = {
    mainGame(player1Playing, playingInSpMode, 6, board)
  }

  def playBottomCentre(): Unit = {
    mainGame(player1Playing, playingInSpMode, 7, board)
  }

  def playBottomRight(): Unit = {
    mainGame(player1Playing, playingInSpMode, 8, board)
  }

  def exit(): Unit = System.exit(1)

}