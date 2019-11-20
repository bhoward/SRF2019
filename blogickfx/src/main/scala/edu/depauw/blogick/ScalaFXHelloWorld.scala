package edu.depauw.blogick

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.effect.DropShadow
import scalafx.scene.layout.HBox
import scalafx.scene.paint.Color._
import scalafx.scene.paint._
import scalafx.scene.text.Text

import edu.depauw.blogick.model._
import _root_.edu.depauw.blogick.gui.RenderProof

object ScalaFXHelloWorld extends JFXApp {
  val A = Proposition("A")
  val B = Proposition("B")
  val C = Proposition("C")
  val bindingH1 = Binding("H1", Implication(Conjunction(A, B), C))
  val bindingH2 = Binding("H2", Conjunction(B, A))
  val proof = ImplIntro(
    bindingH1,
    ImplIntro(
      bindingH2,
      ImplElim(
        Use(bindingH1),
        ConjIntro(
          ConjElimSecond(Use(bindingH2)),
          ConjElimFirst(Use(bindingH2))
        )
      )
    )
  )
  val cp = proof.check(Nil)


  stage = new PrimaryStage {
    //    initStyle(StageStyle.Unified)
    title = "Block Logic"
    scene = new Scene {
      content = RenderProof(cp)
      // fill = Color.rgb(38, 38, 38)
      // content = new HBox {
      //   padding = Insets(50, 80, 50, 80)
      //   children = Seq(
      //     new Text {
      //       text = "Scala"
      //       style = "-fx-font: normal bold 100pt sans-serif"
      //       fill = new LinearGradient(
      //         endX = 0,
      //         stops = Stops(Red, DarkRed))
      //     },
      //     new Text {
      //       text = "FX"
      //       style = "-fx-font: italic bold 100pt sans-serif"
      //       fill = new LinearGradient(
      //         endX = 0,
      //         stops = Stops(White, DarkGray)
      //       )
      //       effect = new DropShadow {
      //         color = DarkGray
      //         radius = 15
      //         spread = 0.25
      //       }
      //     }
      //   )
      // }
    }

  }
}
