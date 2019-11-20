package edu.depauw.blogick.gui

import edu.depauw.blogick.model._
import scalafx.geometry.Insets
import scalafx.scene.Node
import scalafx.scene.control.TitledPane
import scalafx.scene.layout.VBox
import scalafx.scene.layout.HBox
import scalafx.scene.paint._
import scalafx.scene.text.Text


object RenderProof {
  def txt(s: String): Node = new Text(s) {
    margin = Insets(10, 10, 10, 10)
  }

  def apply(proof: CheckedProof): Node = proof match {
    case CkConjElimFirst(formula, conj) => new TitledPane {
      text = formula.toString
      style = "-fx-background: #66f"

      content = new VBox(txt("first of"), RenderProof(conj))
    }

    case CkConjElimSecond(formula, conj) => new TitledPane {
      text = formula.toString
      style = "-fx-background: #66f"

      content = new VBox(txt("second of"), RenderProof(conj))
    }

    case CkConjIntro(formula, first, second) => new TitledPane {
      text = formula.toString
      style = "-fx-background: hsb(240, 50%, 100%)"

      content = new HBox(RenderProof(first),
      txt("and"),
      RenderProof(second))
    }

    case CkDisjElim(formula, disj, leftBind, leftCase, rightBind, rightCase) => ???

    case CkDisjIntroLeft(formula, arg) => ???

    case CkDisjIntroRight(formula, arg) => ???

    case CkFalseElim(formula, falsum) => ???

    case CkImplElim(formula, impl, arg) => new TitledPane {
      text = formula.toString
      style = "-fx-background: hsb(120, 20%, 100%)"

      content = new HBox(RenderProof(impl), txt("apply to"), RenderProof(arg))
    }

    case CkImplIntro(formula, hypothesis, conclusion) => new TitledPane {
      text = formula.toString
      style = "-fx-background: hsb(120, 100%, 100%)"

      content = new VBox(txt(s"assume ${hypothesis.name} : ${hypothesis.formula}"), RenderProof(conclusion))
    }

    case CkNegElim(formula, neg, arg) => ???

    case CkNegIntro(formula, hypothesis, contradiction) => ???

    case CkToDo(formula, env) => ???

    case CkTrueIntro => ???

    case CkUse(formula, binding) => new TitledPane {
      text = formula.toString
      style = "-fx-background: #ff2"

      content = txt(s"use ${binding.name} : ${binding.formula}")
    }
  }
}