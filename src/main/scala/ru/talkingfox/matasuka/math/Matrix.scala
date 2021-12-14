package ru.talkingfox.matasuka.math

case class Matrix(core: List[List[MathExpression]], rows: Int, cols: Int) {
  def addRow(row: List[MathExpression]): Matrix =
    copy(core = core.appended(row), rows = rows + 1)

  def addCol(col: List[MathExpression]): Matrix =
    copy(core = core.zip(col).map{
      case (list, appended) => appended :: list
    }, rows, cols + 1)

  def transponse: Matrix =
    core.foldLeft(Matrix.empty) {
      case (matrix, row) => matrix.addCol(row)
    }
}
object Matrix {
  def empty: Matrix = Matrix(Nil, 0, 0)
}
object Main extends App {

}
