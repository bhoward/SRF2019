package edu.depauw.blogick.gui

import edu.depauw.blogick.model._
import scalafx.geometry.Insets
import scalafx.scene.Node
import scalafx.scene.control.TitledPane
import scalafx.scene.layout.VBox
import scalafx.scene.layout.HBox
import scalafx.scene.paint._
import scalafx.scene.text.Text
import scalafx.scene.layout.Pane
import scalafx.geometry.Pos


object RenderProof {
  def txt(s: String): Node = new Text(s) {
    margin = Insets(10, 10, 10, 10)
  }

  def boxtxt(s: String): Node = new HBox(new Text(s) { margin = Insets(2, 2, 2, 2) }) {
    style = "-fx-background-color: white; -fx-border-color: black; -fx-border-radius: 2"
  }

  def txtRow(nodes: Node*): Node = new HBox(nodes : _*) { fillHeight = false; alignment = Pos.CenterLeft }

  def apply(proof: CheckedProof): Node = proof match {
    case CkConjElimFirst(formula, conj) => new TitledPane {
      text = formula.toString
      style = "-fx-background: derive(dodgerblue, 50%)"

      content = new VBox(txt("first of"), RenderProof(conj))
    }

    case CkConjElimSecond(formula, conj) => new TitledPane {
      text = formula.toString
      style = "-fx-background: derive(dodgerblue, 50%)"

      content = new VBox(txt("second of"), RenderProof(conj))
    }

    case CkConjIntro(formula, first, second) => new TitledPane {
      text = formula.toString
      style = "-fx-background: dodgerblue"

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
      style = "-fx-background: derive(limegreen, 50%)"

      content = new HBox(RenderProof(impl), txt("apply to"), RenderProof(arg))
    }

    case CkImplIntro(formula, hypothesis, conclusion) => new TitledPane {
      text = formula.toString
      style = "-fx-background: limegreen"

      content = new VBox(
        txtRow(txt("assume"), boxtxt(s"${hypothesis.name} : ${hypothesis.formula}")),
        RenderProof(conclusion))
    }

    case CkNegElim(formula, neg, arg) => ???

    case CkNegIntro(formula, hypothesis, contradiction) => ???

    case CkToDo(formula, env) => ???

    case CkTrueIntro => ???

    case CkUse(formula, binding) => new TitledPane {
      text = formula.toString
      style = "-fx-background: #ff2"

      content = txtRow(txt("use"), boxtxt(s"${binding.name} : ${binding.formula}"))
    }
  }
}