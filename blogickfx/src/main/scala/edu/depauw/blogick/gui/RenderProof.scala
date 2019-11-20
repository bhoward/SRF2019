package edu.depauw.blogick.gui

import edu.depauw.blogick.model._
import scalafx.scene.Node
import scalafx.scene.control.TitledPane
import scalafx.scene.layout.VBox
import scalafx.scene.layout.HBox
import scalafx.scene.text.Text

object RenderProof {
  def apply(proof: CheckedProof): Node = proof match {
    case CkConjElimFirst(formula, conj) => new TitledPane {
      text = formula.toString

      content = new VBox(new Text("first of"), RenderProof(conj))
    }

    case CkConjElimSecond(formula, conj) => new TitledPane {
      text = formula.toString

      content = new VBox(new Text("second of"), RenderProof(conj))
    }

    case CkConjIntro(formula, first, second) => new TitledPane {
      text = formula.toString

      content = new HBox(RenderProof(first), new Text("and"), RenderProof(second))
    }

    case CkDisjElim(formula, disj, leftBind, leftCase, rightBind, rightCase) => ???

    case CkDisjIntroLeft(formula, arg) => ???

    case CkDisjIntroRight(formula, arg) => ???

    case CkFalseElim(formula, falsum) => ???

    case CkImplElim(formula, impl, arg) => ???

    case CkImplIntro(formula, hypothesis, conclusion) => new TitledPane {
      text = formula.toString

      content = new VBox(new Text(s"assume ${hypothesis.name} : ${hypothesis.formula}"), RenderProof(conclusion))
    }

    case CkNegElim(formula, neg, arg) => ???

    case CkNegIntro(formula, hypothesis, contradiction) => ???

    case CkToDo(formula, env) => ???

    case CkTrueIntro => ???

    case CkUse(formula, binding) => new TitledPane {
      text = formula.toString

      content = new Text(s"use ${binding.name} : ${binding.formula}")
    }
  }
}