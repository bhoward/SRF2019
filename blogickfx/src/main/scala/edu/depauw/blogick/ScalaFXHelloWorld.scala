package edu.depauw.blogick

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene

import edu.depauw.blogick.gui.ProofRenderer
import edu.depauw.blogick.model._

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
          // ConjElimFirst(Use(bindingH2))
          ToDo
        )
      )
    )
  )
  val cp = proof.check(Nil)

  stage = new PrimaryStage {
    title = "Block Logic"
    scene = new Scene {
      content = ProofRenderer(cp)
    }
  }
}
