package com.michaelmaysonet74.handsonscala.chapters

import scala.concurrent.{ExecutionContext, Future}

final case class Chapter4()(implicit val ec: ExecutionContext) {

  type Block = List[String]
  type Row = List[Int]
  type Grid = List[Row]

  def execute(): Future[Unit] = {
    val validGrid = List(
      List(3, 1, 6, 5, 7, 8, 4, 9, 2),
      List(5, 2, 9, 1, 3, 4, 7, 6, 8),
      List(4, 8, 7, 6, 2, 9, 5, 3, 1),
      List(2, 6, 3, 0, 1, 0, 0, 8, 0),
      List(9, 7, 4, 8, 6, 3, 0, 0, 5),
      List(8, 5, 1, 0, 9, 0, 6, 0, 0),
      List(1, 3, 0, 0, 0, 0, 2, 5, 0),
      List(0, 0, 0, 0, 0, 0, 0, 7, 4),
      List(0, 0, 5, 2, 0, 6, 3, 0, 0)
    )

    isValidSudoku(validGrid).map(println) // => true

    isValidSudoku(
      List(
        List(3, 1, 6, 5, 7, 8, 4, 9, 3),
        List(5, 2, 9, 1, 3, 4, 7, 6, 8),
        List(4, 8, 7, 6, 2, 9, 5, 3, 1),
        List(2, 6, 3, 0, 1, 0, 0, 8, 0),
        List(9, 7, 4, 8, 6, 3, 0, 0, 5),
        List(8, 5, 1, 0, 9, 0, 6, 0, 0),
        List(1, 3, 0, 0, 0, 0, 2, 5, 0),
        List(0, 0, 0, 0, 0, 0, 0, 7, 4),
        List(0, 0, 5, 2, 0, 6, 3, 0, 0)
      )
    ).map(println)
    // => false, top right cell should be 2

    renderSudoku(validGrid).map(println)
  }

  def isValidSudoku(grid: Grid): Future[Boolean] = Future {
    !Range(0, 9).exists { i =>
      // Filter out 0 values, before checking for unique values
      val row = Range(0, 9).map(grid(i)(_)).filter(_ > 0)
      val col = Range(0, 9).map(grid(_)(i)).filter(_ > 0)
      val square = Range(0, 9)
        .map(j => grid((i % 3) * 3 + j % 3)((i / 3) * 3 + j / 3))
        .filter(_ > 0)

      row.distinct.length != row.length ||
      col.distinct.length != col.length ||
      square.distinct.length != square.length
    }
  }

  def renderSudoku(grid: Grid): Future[String] = Future.successful(
    (renderRows _ andThen renderBlocks _)(grid).replaceAll("0", " ")
  )

  private def renderRow(row: Row): String =
    row
      .grouped(3)
      .map(_.mkString("| ", " ", " "))
      .mkString("", "", "|")

  private def renderRows(grid: Grid): List[Block] = {
    grid
      .map(renderRow)
      .grouped(3)
      .toList
  }

  private def renderBlocks(blocks: List[Block]): String =
    blocks
      .map(
        _.mkString(
          "+-------+-------+-------+\n",
          "\n",
          "\n"
        )
      )
      .mkString(
        "",
        "",
        "+-------+-------+-------+"
      )

}
