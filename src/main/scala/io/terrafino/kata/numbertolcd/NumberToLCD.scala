package io.terrafino.kata.numbertolcd

class NumberToLCD(factor:Int = 1) {

  def convert(number: Int): String = {
    val lcdDigits = number.toString.map(c => convertDigit(c.asDigit))
    val lcdNumber = lcdDigits.reduce((d1, d2) => combine(d1, d2))
    getStringFor(resize(lcdNumber))
  }

  def resize(lcdNumber: LCDNumber): LCDNumber = {
    val newRows = lcdNumber.rows.toList.flatMap(resize)
    LCDNumber(newRows: _*)
  }

  def resize(row: Row): List[Row] = {
    val resizedCells: List[List[List[Cell]]] = row.cells.toList.map(resize)
    val allRows: List[List[Row]] = for (cell <- resizedCells) yield cell.map{ case (cells) => Row(cells:_*)}
    val res: List[Row] = allRows.reduce {
      (r1: List[Row], r2: List[Row]) =>
        r1.zip(r2).map {
          case (row1, row2) => Row(row1.cells ++ row2.cells:_*)
        }
    }
    res
  }

  def resize(cell: Cell): List[List[Cell]] = List.tabulate(factor, factor){(r: Int, c: Int) => go(r, c, cell)}

  def go(row: Int, col: Int, cell: Cell) : Cell = cell match {
    case Dash() => if (row < factor - 1) Blank() else Dash()
    case LBar() => if (col < factor - 1) Blank() else LBar()
    case RBar() => if (col == 0) RBar() else Blank()
    case _ => Blank()
  }

  def combine(d1: LCDNumber, d2: LCDNumber) = {
    val rowsZipped: List[(Row, Row)] = d1.rows.toList zip d2.rows.toList
    val combinedRows = rowsZipped map { case (r1, r2) => Row(r1.cells.toList ++ List(Blank()) ++ r2.cells: _*) }
    LCDNumber(combinedRows: _*)
  }

  private def getStringFor(lcdNumber: LCDNumber): String = {
    lcdNumber.rows.map(getStringFor).mkString("")
  }


  private def getStringFor(row: Row): String = {
    row.cells.toList.map(getStringFor).mkString("", "", "\n")
  }

  private def getStringFor(cell: Cell): String = cell match {
    case Blank() => " "
    case Dash() => "_"
    case LBar() => "|"
    case RBar() => "|"
  }

  private def convertDigit(i: Int): LCDNumber = i match {
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
