package edu.depauw.blogick

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene

import edu.depauw.blogick.gui.ProofRenderer
import edu.depauw.blogick.model._

object ScalaFXHelloWorld extends JFXApp {
  val proof = ImplIntro(
    "H1",
    ImplIntro(
      "H2",
      ImplElim(
        Use("H1"),
        ConjIntro(
          ConjElimSecond(Use("H2")),
          ConjElimFirst(Use("H2"))
          // ToDo
        )
      )
    )
  )

  val cp = proof.check.runA(Environment.Empty).value

  stage = new PrimaryStage {
    title = "Block Logic"
    scene = new Scene {
      content = ProofRenderer(cp)
    }
  }
}
