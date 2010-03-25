case class Context(context: Map[String, Int]) {
  def get(name: String): Int = context(name)
}

abstract class Expression {
  def interpret(ctx: Context): Int
}

case class Digit(value: Int) extends Expression {
  override def interpret(ctx: Context): Int = value
}

case class Variable(name: String) extends Expression {
  override def interpret(ctx: Context): Int = ctx.get(name)
}

case class Plus(lh: Expression, rh: Expression) extends Expression {
  override def interpret(ctx: Context): Int = lh.interpret(ctx) + rh.interpret(ctx)
}

case class Times(lh: Expression, rh: Expression) extends Expression {
  override def interpret(ctx: Context): Int = lh.interpret(ctx) * rh.interpret(ctx)
}

object Interpreter extends Application {
  val exp = Plus ( Digit(9), Times ( Variable("a"), Digit(4) ) )
  val ctx = Context(Map("a" -> 3))
  println("a 4 * 9 + = " + exp.interpret(ctx))
}

