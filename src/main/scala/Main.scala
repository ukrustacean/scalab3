import processing.core.*

@main def main(): Unit =
  PApplet.runSketch(Array("Sketch$"), Sketch)

object Sketch extends PApplet:
  // Imports
  import PApplet.*
  import PConstants.*

  // Extensions
  extension (v: PVector)
//    inline def +(u: PVector): PVector = PVector.add(v, u)
    inline def -(u: PVector): PVector = PVector.sub(v, u)
    inline def unary_- : PVector = PVector(-v.x, -v.y, -v.z)
    inline infix def lineTo(u: PVector): Unit = line(v.x, v.y, u.x, u.y)

  // Constants
  private val VARIANT = 3106
  private val n = Array(0, 3, 1, 0, 6)
  private val k = 1.0 - n(3) * 0.02 - n(4) * 0.005 - 0.25
  private val N = n(3) + 10

  // Don't forget to seed RNG!
  randomSeed(VARIANT)

  // Graph data
  private val matrix = Array.fill(N, N) { 1 <= random(2) * k }
  private val unimatrix =
    for i <- (0 until N).toArray yield
      for j <- (0 until N).toArray yield
        matrix(i)(j) || matrix(j)(i)
  private val points: Array[PVector] =
    (1 until N map { TWO_PI * _ / (N - 1) } map { x =>
      PVector(cos(x) * 280, sin(x) * 280)
    }).toArray :+ PVector(0, 0)

  // Global state
  private var directed = true

  // Printing routines
  def displayMatrix(directed: Boolean = true): Unit =
    val m = if directed then matrix else unimatrix

    for row <- m do
      for i <- row do print(s"${if i then '1' else '0'} ")
      println()

  // Drawing routines
  override def settings(): Unit = size(700, 700)

  override def setup(): Unit =

    println("Directed graph:")
    displayMatrix()
    println("\nUndirected graph:")
    displayMatrix(directed = false)

    windowTitle("Circle Graph")

    colorMode(HSB, 360, 100, 100)
    strokeWeight(2)
    stroke(255)

    textAlign(CENTER, CENTER)
    textSize(40)

  override def draw(): Unit =
    background(10)
    translate(width / 2, height / 2)

    for
      (point, i) <- points.zipWithIndex
      (edge, j) <- matrix(i).zipWithIndex
      if edge && i != j
    do
      push()
      {
        translate(point.x, point.y)

        val lineOffset = if matrix(i)(j) && matrix(j)(i) && directed then 3 else 0
        val start = PVector(0, 0)
        val end = points(j) - point
        val dir = -end
        val offset = dir.copy()
        offset.setMag(30)

        rotate(end.heading)
        if directed then arrow(PI, PVector(end.mag - 30, lineOffset))
        line(0, lineOffset, end.mag(), lineOffset)
      }
      pop()

    for (point, i) <- points.zipWithIndex do
      push()
      {
        translate(point.x, point.y)

        if matrix(i)(i) then
          noFill()
          circle(0, -40.83f, 40)
          if directed then arrow(-1f, 14f, -26.5f)

        fill(100)
        circle(0, 0, 60)
        fill(255)
        text(i + 1, 0, 0)
      }
      pop()

  inline private def arrow(phi: Float, x: Float, y: Float): Unit =
    arrow(phi, PVector(x, y))

  inline private def arrow(dir: PVector, p: PVector): Unit =
    arrow(dir.heading, p)

  private def arrow(phi: Float, p: PVector): Unit =
    p lineTo PVector(p.x + 15 * cos(phi + 0.3f), p.y + 15 * sin(phi + 0.3f))
    p lineTo PVector(p.x + 15 * cos(phi - 0.3f), p.y + 15 * sin(phi - 0.3f))

  override def keyPressed(): Unit =
    directed = if key == ' ' then !directed else directed