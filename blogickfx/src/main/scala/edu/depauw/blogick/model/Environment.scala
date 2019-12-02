package edu.depauw.blogick.model

import cats.data.State

final case class Binding(name: String, formula: Formula)

object Environment {
  val Empty: Environment = new Environment(Nil, 0)

  def extend(name: String, f: Formula): State[Environment, Unit] = State { env =>
    (env.extend(name, f), ())
  }

  val retract: State[Environment, Binding] = State { env =>
    env.retract()
  }

  val genVar: State[Environment, Formula] = State { env =>
    env.genVar()
  }
}

class Environment(val bindings: List[Binding], varId: Int) {
  def apply(name: String): Option[Formula] = {
    bindings.find(_.name == name).map(_.formula)
  }

  def extend(name: String, f: Formula): Environment = {
    new Environment(Binding(name, f) :: bindings, varId)
  }

  def retract(): (Environment, Binding) = bindings match {
    case binding :: rest => (new Environment(rest, varId), binding)
    case _ => sys.error("No bindings left in environment")
  }

  def genVar(): (Environment, Variable) = {
    val newVarId = varId + 1
    (new Environment(bindings, newVarId), Variable(newVarId))
  }
}