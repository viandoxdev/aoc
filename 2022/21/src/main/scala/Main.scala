import java.net.http
import java.net.URI
import scala.io.Source
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.NoSuchFileException
import java.nio.file.StandardOpenOption

enum Operator:
  case Add, Sub, Mul, Div, Equals, None
  override def toString(): String = this match
    case Operator.Add => "+"
    case Operator.Sub => "-"
    case Operator.Mul => "*"
    case Operator.Div => "/"
    case Operator.Equals => "="
    case Operator.None => ""

  def inv: Operator = this match
    case Operator.Add => Operator.Sub
    case Operator.Sub => Operator.Add
    case Operator.Mul => Operator.Div
    case Operator.Div => Operator.Mul
    case any => any

  def exec(lhs: Long, rhs: Long): Long = this match
    case Operator.Add => lhs + rhs
    case Operator.Sub => lhs - rhs
    case Operator.Mul => lhs * rhs
    case Operator.Div => lhs / rhs
    case Operator.Equals | Operator.None => 0


object Operator:
  def parse(s: String): Operator = s match
    case "+" => Operator.Add
    case "-" => Operator.Sub
    case "*" => Operator.Mul
    case "/" => Operator.Div

class Expr(var lhs: Expr, var rhs: Expr, var op: Operator):
  private var value: Option[Long] = None

  def this() = 
    this(null, null, Operator.None)
  
  def this(v: Long) =
    this(null, null, Operator.None)
    value = Some(v)

  def isKnown: Boolean = !value.isEmpty

  def get: Long = value.get

  // redorder lhs and rhs so that if at least one is known, then rhs is known
  def reorder(): Unit =
    if lhs.isKnown && !rhs.isKnown then
      val old = lhs
      lhs = rhs
      rhs = old

  def simplify(): Boolean =
    if lhs == null then
      false
    else
      if lhs.isKnown && rhs.isKnown then
        value = Some(op.exec(lhs.get, rhs.get))
        lhs = null
        rhs = null
        op = Operator.None
        true
      else
        lhs.simplify() || rhs.simplify()

  def move(): Boolean =
    if op != Operator.Equals || !rhs.isKnown || lhs.isKnown || lhs.op == Operator.None || (!lhs.lhs.isKnown && !lhs.rhs.isKnown) then
      false
    else
      // We have: An equality between a a known rhs expr, and a unknown lhs expr that has one known argument
      if lhs.lhs.isKnown && !lhs.rhs.isKnown then 
        if lhs.op == Operator.Add || lhs.op == Operator.Mul then
          rhs = Expr(lhs.op.inv.exec(rhs.get, lhs.lhs.get))
          lhs = lhs.rhs
        else
          rhs = Expr(lhs.op.exec(lhs.lhs.get, rhs.get))
          lhs = lhs.rhs
      else 
        rhs = Expr(lhs.op.inv.exec(rhs.get, lhs.rhs.get))
        lhs = lhs.lhs
      true

  override def toString(): String = 
    if op == Operator.None then
      if isKnown then
        value.get.toString
      else
        "x"
    else
      s"($lhs $op $rhs)"

case class Formula(lhs: String, rhs: String, op: Operator):
  private var value: Option[Long] = None

  def this(v: Long) =
    this(null, null, Operator.None)
    value = Some(v)

  override def toString(): String = 
    s"$lhs $op $rhs"

  def isKnown: Boolean = !value.isEmpty

  def forget(): Unit =
    if lhs != null then
      value = None

  def toExpr(data: Data): Expr =
    if lhs == null then
      Expr(value.get)
    else
      Expr(data.getMonkeyExpr(lhs), data.getMonkeyExpr(rhs), op)

  def exec(data: Data): Long =
    if value.isEmpty then
      val res = op.exec(data.getMonkeyValue(lhs), data.getMonkeyValue(rhs))
      value = Some(res)
      res
    else
      value.get

class Data:
  private var monkeys: Map[String, Formula] = Map()

  def parse(input: String): Unit =
    monkeys = input.split("\n").map(line => 
      val fields = line.split(":? ")
      val cur = fields(0)
      if fields.length > 2 then
        (cur, Formula(fields(1), fields(3), Operator.parse(fields(2))))
      else
        (cur, new Formula(fields(1).toLong))
    ).toMap

  def getMonkeyValue(monkey: String): Long =
    monkeys(monkey).exec(this)

  def getMonkeyExpr(monkey: String): Expr =
    monkeys.get(monkey).map(_.toExpr(this)).orElse(Some(new Expr())).get

  def fix(): Unit =
    for (_, m) <- monkeys do
      m.forget()
    monkeys = monkeys.updatedWith("root")(prev => Some(Formula(prev.get.lhs, prev.get.rhs, Operator.Equals)))
    monkeys = monkeys.removed("humn")

object Aoc:
  def fetch_input: String =
    val reader = Source.fromFile("../../session")
    val session = reader.getLines.next
    reader.close
    val req = http.HttpRequest.newBuilder()
      .header("Cookie", s"session=${session}")
      .GET()
      .uri(new URI("https://adventofcode.com/2022/day/21/input"))
      .build()
    http.HttpClient.newHttpClient()
      .send(req, http.HttpResponse.BodyHandlers.ofString())
      .body()

  def input: String =
    val path = Path.of("input")
    try
      Files.readString(path)
    catch
      case _: NoSuchFileException =>
        val input = fetch_input
        Files.writeString(path, input, StandardOpenOption.CREATE)
        input
      case any => throw any

@main def aoc21: Unit = 
  val input = Aoc.input
  val data = Data()
  data.parse(input)

  val part1 = data.getMonkeyValue("root")

  data.fix()

  var e = data.getMonkeyExpr("root")
  while e.simplify() do
    ()
  while e.move() do
    println(s"$e")

  val part2 = e.rhs.get

  println(s"Part1: $part1")
  println(s"Part2: $part2")
