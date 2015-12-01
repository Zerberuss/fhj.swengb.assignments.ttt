package fhj.swengb.assignments.ttt.aschneider

import javafx.application.Application
import javafx.scene.shape.{CubicCurveTo, MoveTo, Path}
import javafx.stage.Stage
import java.awt.Button
import java.beans.EventHandler
import java.net.URL
import java.rmi.activation.ActivationGroup_Stub
import java.util.ResourceBundle
import javafx.animation._
import javafx.application.Application
import javafx.fxml.{FXML, FXMLLoader, Initializable}
import javafx.scene.text
import javafx.scene.control.Label
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout.{AnchorPane, GridPane, BorderPane}
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage
import javafx.util.Duration
import com.sun.javaws.jnl.JavaFXAppDesc
import scala.util.control.NonFatal


//object launches the class described beneath
object TicTacToeApp {
  def main(args: Array[String]) {
    Application.launch(classOf[TicTacToeApp], args: _*)
  }
}


class TicTacToeApp extends javafx.application.Application {


  val Fxml = "fhj/swengb/assignments/ttt/aschneider/TicTacToeApp.fxml"
  val Css = "fhj/swengb/assignments/ttt/TicTacToe.css"

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
class TicTacToeAppController extends Initializable {
  //attributes are being initialized (everything with an ID)
  @FXML var menue: AnchorPane = _
  @FXML var mpMenue: AnchorPane = _
  @FXML var spMenue: AnchorPane = _
  @FXML var spMenueBack: Button = _
  @FXML var mpMenueBack: Button = _
  @FXML var spStartMenue: Button = _
  @FXML var mpStartMenue: Button = _
  @FXML var mpStart: Button = _
  @FXML var spStart: Button = _


  @FXML var exit: Button = _

  @FXML var user0f: ImageView = _



  @FXML var user1f: ImageView = _
  @FXML var user2f: ImageView = _
  @FXML var user3f: ImageView = _
  @FXML var user4f: ImageView = _
  @FXML var user5f: ImageView = _
  @FXML var user6f: ImageView = _
  @FXML var user7f: ImageView = _
  @FXML var user8f: ImageView = _
  @FXML var user9f: ImageView = _
  @FXML var user10f: ImageView = _
  @FXML var user11f: ImageView = _
  @FXML var username0: Label = _
  @FXML var username1: Label = _
  @FXML var username2: Label = _
  @FXML var username3: Label = _
  @FXML var username4: Label = _
  @FXML var username5: Label = _
  @FXML var username6: Label = _
  @FXML var username7: Label = _
  @FXML var username8: Label = _
  @FXML var username9: Label = _
  @FXML var username10: Label = _
  @FXML var username11: Label = _

  @FXML var githubname: Label = _
  @FXML var avatar: ImageView = _
  @FXML var fullname: Label = _
  @FXML var group: Label = _
  @FXML var publicrepos: Label = _
  @FXML var followers: Label = _
  @FXML var following: Label = _


  //initialize function executes the commands at startup for the main scene

    def anim(obj: AnchorPane, slideRight: Boolean): Unit = {
      var xMitte = 300
      var yMitte = 400
      var path: Path = new Path()

      if (slideRight) {
        path.getElements.add(new MoveTo(xMitte, yMitte))
        path.getElements().add(new CubicCurveTo(xMitte + 50, yMitte, xMitte + 200, yMitte, xMitte + 700, yMitte))
      } else {
        path.getElements.add(new MoveTo(xMitte + 700, yMitte))
        path.getElements().add(new CubicCurveTo(xMitte + 200, yMitte, xMitte + 50, yMitte, xMitte, yMitte))
      }

      var pathTrans: PathTransition = new PathTransition()
      pathTrans.setDuration(new Duration(200))
      pathTrans.setNode(obj)
      pathTrans.setPath(path)
      pathTrans.setAutoReverse(false)
      pathTrans.play()
    }




    def mpMenueBack(): Unit = anim(mpMenue, true)
    //Hier die richtigen User Infos übergeben! -> aus tabelle oda ka wie ihr sie gespeichert habts
    def spMenueBack(): Unit = anim(spMenue, true)

    def mpStartMenue(): Unit = anim(mpMenue, false)

    def spStartMenue(): Unit = anim(spMenue, false)

    def spStart(): Unit = anim(spMenue, false)

    def mpStart(): Unit = anim(spMenue, false)




}