package edu.depauw.blogick

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene

import edu.depauw.blogick.gui.ProofRenderer
import edu.depauw.blogick.model._

object BlogickFX extends JFXApp {
  // val thm = Theorem.fromString("demo: ((P & Q) -> R) -> (Q & P) -> R = H1 => H2 => H1 <second H2, first H2>")
  val thm = Theorem.fromString("demo: ((P & Q) -> R) -> (Q & P) -> R = H1 => H2 => H1 <second H2, ?>")
  val thm2 = Theorem.fromString("demo2: (⊥ → P ∨ ⊥ → Q) → ⊥ → ((Q → P ∧ R) → P ∧ (S ∨ T)) = x ⇒ y ⇒ <[left a ⇒ b ⇒ a y, right a ⇒ b ⇒ first b (a y)] x, left ([] y)>")

  stage = new PrimaryStage {
    title = s"Block Logic: $thm2"
    scene = new Scene {
      content = ProofRenderer(thm2.checkedProof)
    }
  }
}
