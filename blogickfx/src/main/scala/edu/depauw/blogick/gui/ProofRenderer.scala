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
import scalafx.scene.input.TransferMode
import scalafx.scene.input.DataFormat
import scalafx.scene.input.ClipboardContent
import scalafx.scene.image.WritableImage
import scalafx.scene.SnapshotResult
import scalafx.scene.control.ContextMenu
import scalafx.scene.control.MenuItem

object ProofRenderer {
  def txt(s: String): Node = new Text(s) {
    margin = Insets(10, 10, 10, 10)
  }

  def boxtxt(s: String): Node =
    new HBox(new Text(s) { margin = Insets(2, 2, 2, 2) }) {
      style =
        "-fx-background-color: white; -fx-border-color: black; -fx-border-radius: 2"
    }

  def txtRow(nodes: Node*): Node = new HBox(nodes: _*) {
    fillHeight = false
    alignment = Pos.CenterLeft
  }

  def apply(proof: CheckedProof): Node = proof match {
    case CkConjElimFirst(formula, conj) =>
      new TitledPane {
        text = formula.toString
        style = "-fx-background: derive(dodgerblue, 50%)"

        content = new VBox(txt("first of"), ProofRenderer(conj))
      }

    case CkConjElimSecond(formula, conj) =>
      new TitledPane {
        text = formula.toString
        style = "-fx-background: derive(dodgerblue, 50%)"

        content = new VBox(txt("second of"), ProofRenderer(conj))
      }

    case CkConjIntro(formula, first, second) =>
      new TitledPane {
        text = formula.toString
        style = "-fx-background: dodgerblue"

        content =
          new HBox(ProofRenderer(first), txt("and"), ProofRenderer(second))
      }

    case CkDisjElim(formula, disj, leftBind, leftCase, rightBind, rightCase) =>
      new TitledPane {
        text = formula.toString
        style = "-fx-background: derive(orange, 50%)"

        val lhyp = boxtxt(s"${leftBind.name} : ${leftBind.formula}")
        lhyp.setOnDragDetected { e =>
          val img = lhyp.snapshot(null, null)
          val db = lhyp.startDragAndDrop(TransferMode.Copy)
          db.setContent(ClipboardContent(DataFormat.PlainText -> "Hello")) // TODO
          db.setDragView(
            img,
            img.getWidth / 2 - e.getX,
            e.getY - img.getHeight / 2
          ) // Why?!?
          e.consume()
        }

        val lbox = new VBox(txtRow(txt("left"), lhyp), ProofRenderer(leftCase))

        val rhyp = boxtxt(s"${rightBind.name} : ${rightBind.formula}")
        rhyp.setOnDragDetected { e =>
          val img = rhyp.snapshot(null, null)
          val db = rhyp.startDragAndDrop(TransferMode.Copy)
          db.setContent(ClipboardContent(DataFormat.PlainText -> "Hello")) // TODO
          db.setDragView(
            img,
            img.getWidth / 2 - e.getX,
            e.getY - img.getHeight / 2
          ) // Why?!?
          e.consume()
        }

        val rbox =
          new VBox(txtRow(txt("right"), rhyp), ProofRenderer(rightCase))

        content = new VBox(
          txtRow(txt("case"), ProofRenderer(disj)),
          new HBox(lbox, txt("or"), rbox)
        )
      }

    case CkDisjIntroLeft(formula, arg) =>
      new TitledPane {
        text = formula.toString
        style = "-fx-background: orange"

        content = new VBox(txt("left"), ProofRenderer(arg))
      }

    case CkDisjIntroRight(formula, arg) =>
      new TitledPane {
        text = formula.toString
        style = "-fx-background: orange"

        content = new VBox(txt("right"), ProofRenderer(arg))
      }

    case CkFalseElim(formula, falsum) =>
      new TitledPane {
        text = formula.toString
        style = "-fx-background: derive(purple, 50%)"

        content = new VBox(txt("absurd"), ProofRenderer(falsum))
      }

    case CkImplElim(formula, impl, arg) =>
      new TitledPane {
        text = formula.toString
        style = "-fx-background: derive(limegreen, 50%)"

        content =
          new HBox(ProofRenderer(impl), txt("apply to"), ProofRenderer(arg))
      }

    case CkImplIntro(formula, hypothesis, conclusion) =>
      new TitledPane {
        text = formula.toString
        style = "-fx-background: limegreen"

        val hyp = boxtxt(s"${hypothesis.name} : ${hypothesis.formula}")
        hyp.setOnDragDetected { e =>
          val img = hyp.snapshot(null, null)
          val db = hyp.startDragAndDrop(TransferMode.Copy)
          db.setContent(ClipboardContent(DataFormat.PlainText -> "Hello")) // TODO
          db.setDragView(
            img,
            img.getWidth / 2 - e.getX,
            e.getY - img.getHeight / 2
          ) // Why?!?
          e.consume()
        }

        content =
          new VBox(txtRow(txt("assume"), hyp), ProofRenderer(conclusion))
      }

    case CkToDo(formula, env) =>
      new TitledPane {
        text = formula.toString
        style = "-fx-background: red"

        // TODO populate this properly
        contextMenu = new ContextMenu(
          new MenuItem("Use ..."),
          new MenuItem("Elim ..."),
          new MenuItem("Intro ...")
        )

        // TODO make this bigger? allow dropping hypotheses here?
        // make this a workspace where subproofs can be built using env?
        content = new Pane {
          prefHeight = 100
          prefWidth = 100
        }
      }

    case CkTrueIntro =>
      new TitledPane {
        text = True.toString
      }

    case CkUse(formula, binding) =>
      new TitledPane {
        text = formula.toString
        style = "-fx-background: #ff2"

        content =
          txtRow(txt("use"), boxtxt(s"${binding.name} : ${binding.formula}"))
      }
  }
}
