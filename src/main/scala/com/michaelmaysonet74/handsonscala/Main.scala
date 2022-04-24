package com.michaelmaysonet74.handsonscala

import com.michaelmaysonet74.handsonscala.chapters._

import scala.concurrent.ExecutionContext

object Main extends App {

  implicit val ec = ExecutionContext.global

  val chapter3 = Chapter3()
  val chapter4 = Chapter4()
  val chapter12 = Chapter12()

  chapter3.execute()
  chapter4.execute()
  chapter12.execute()

}
