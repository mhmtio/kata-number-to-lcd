package io.terrafino.kata.numbertolcd

class NumberToLCD(factor: Int = 1) {

  private val CellToString = Map[Cell, String](
    Blank() -> " ",
    Dash() -> "_",
    LBar() -> "|",
    RBar() -> "|"
  )

  def convert(number: Int): String = {
    val lcdDigits = number.toString.map(c => convertDigit(c.asDigit))
    val lcdNumber = lcdDigits.reduce((d1, d2) => combine(d1, d2))
    getStringFor(resize(lcdNumber))
  }

  private def resize(lcdNumber: LCDNumber): LCDNumber = LCDNumber(lcdNumber.rows.flatMap(resize):_*)

  private def resize(row: Row): List[Row] = {
    val resizedCells = row.cells.map(resize)
    val allRows = for (cell <- resizedCells) yield cell.map(Row(_: _*))
    allRows.reduce((rows1, rows2) => rows1.zip(rows2).map(p => combineRows(p._1, p._2)))
  }

  private def resize(cell: Cell) = List.tabulate(factor, factor) {
    (row: Int, col: Int) =>
      cell match {
        case Dash() => if (row < factor - 1) Blank() else Dash()
        case LBar() => if (col < factor - 1) Blank() else LBar()
        case RBar() => if (col == 0) RBar() else Blank()
        case _ => Blank()
      }
  }

  private def combineRows(row1: Row, row2: Row): Row = Row(row1.cells ++ row2.cells: _*)

  private def combine(n1: LCDNumber, n2: LCDNumber): LCDNumber = {
    val rowsZipped = n1.rows zip n2.rows
    val combinedRows = rowsZipped map { case (r1, r2) => Row(r1.cells.toList ++ List(Blank()) ++ r2.cells: _*) }
    LCDNumber(combinedRows: _*)
  }

  private def getStringFor(lcdNumber: LCDNumber): String = lcdNumber.rows.map(getStringFor).mkString("")

  private def getStringFor(row: Row): String = row.cells.map(CellToString(_)).mkString("", "", "\n")

  private def convertDigit(d: Int): LCDNumber = d match {
    case 0 => LCDNumber(Row(Blank(), Dash(), Blank()), Row(LBar(), Blank(), RBar()), Row(LBar(), Dash(), RBar()))
    case 1 => LCDNumber(Row(Blank(), Blank(), Blank()), Row(Blank(), Blank(), RBar()), Row(Blank(), Blank(), RBar()))
    case 2 => LCDNumber(Row(Blank(), Dash(), Blank()), Row(Blank(), Dash(), RBar()), Row(LBar(), Dash(), Blank()))
    case 3 => LCDNumber(Row(Blank(), Dash(), Blank()), Row(Blank(), Dash(), RBar()), Row(Blank(), Dash(), RBar()))
    case 4 => LCDNumber(Row(Blank(), Blank(), Blank()), Row(LBar(), Dash(), RBar()), Row(Blank(), Blank(), RBar()))
    case 5 => LCDNumber(Row(Blank(), Dash(), Blank()), Row(LBar(), Dash(), Blank()), Row(Blank(), Dash(), RBar()))
    case 6 => LCDNumber(Row(Blank(), Dash(), Blank()), Row(LBar(), Dash(), Blank()), Row(LBar(), Dash(), RBar()))
    case 7 => LCDNumber(Row(Blank(), Dash(), Blank()), Row(Blank(), Blank(), RBar()), Row(Blank(), Blank(), RBar()))
    case 8 => LCDNumber(Row(Blank(), Dash(), Blank()), Row(LBar(), Dash(), RBar()), Row(LBar(), Dash(), RBar()))
    case 9 => LCDNumber(Row(Blank(), Dash(), Blank()), Row(LBar(), Dash(), RBar()), Row(Blank(), Dash(), RBar()))
  }
}

trait Cell

case class Blank() extends Cell

case class Dash() extends Cell

case class LBar() extends Cell

case class RBar() extends Cell

case class Row(cells: Cell*)

case class LCDNumber(rows: Row*)
