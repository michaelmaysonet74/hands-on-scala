package com.michaelmaysonet74.handsonscala.chapters

import requests._
import upickle._

import scala.util.{Left, Right}
import scala.concurrent.{ExecutionContext, Future}

final case class Issue(
  id: Int,
  title: String,
  body: String,
  user: String,
  state: String
)

final case class Comment(
  issueId: Int,
  user: String,
  body: String
)

case class Chapter12()(implicit
  val ec: ExecutionContext
) {

  import Chapter3.withFileReader

  def execute(): Future[Unit] = Future {
    val srcRepo = "lihaoyi/requests-scala"
    val destRepo = "michaelmaysonet74/issue-migration"

    getToken(
      "/Users/michael/code/hands-on-scala/github_token.txt"
    ) match {
      case Left(reason) => println(reason)
      case Right(token) =>
        migrateIssuesWithComments(
          srcRepo,
          destRepo,
          token
        )
    }
  }

  private def getToken(fileName: String): Either[String, String] =
    withFileReader(fileName) { reader =>
      reader.readLine
    } match {
      case Some(token) => Right(token)
      case None        => Left("Couldn't get token.")
    }

  private def migrateIssuesWithComments(
    srcRepo: String,
    destRepo: String,
    token: String
  ): Unit = {
    val issues = getIssues(srcRepo, token);
    val comments = getComments(srcRepo, token)

    val issueNumMap = issues
      .map(
        postIssue(
          _,
          srcRepo,
          destRepo,
          token
        )
      )
      .toMap

    issues
      .collect {
        case issue if issue.state == "closed" && issueNumMap.get(issue.id) != None =>
          closeIssue(
            issueNumMap(issue.id).toInt,
            issue.state,
            destRepo,
            token
          )
      }

    comments
      .collect {
        case comment if issueNumMap.get(comment.issueId) != None =>
          postComment(
            comment,
            issueNumMap(comment.issueId).toInt,
            destRepo,
            token
          )
      }
  }

  private def getIssues(srcRepo: String, token: String): List[Issue] =
    fetchPaginated(
      s"https://api.github.com/repos/$srcRepo/issues",
      token,
      None,
      None,
      "state" -> "all"
    ).collect {
      case issue if !issue.obj.contains("pull_request") =>
        Issue(
          id = issue("number").num.toInt,
          title = issue("title").str,
          body = issue("body").str,
          user = issue("user")("login").str,
          state = issue("state").str
        )
    }.sortBy(_.id)

  private def getComments(srcRepo: String, token: String): List[Comment] =
    fetchPaginated(
      s"https://api.github.com/repos/$srcRepo/issues/comments",
      token,
      None,
      None
    ).map { comment =>
      Comment(
        issueId = comment("issue_url").str match {
          case s"https://api.github.com/repos/$srcRepo/issues/$id" => id.toInt
        },
        user = comment("user")("login").str,
        body = comment("body").str
      )
    }

  private def postIssue(
    issue: Issue,
    srcRepo: String,
    destRepo: String,
    token: String
  ): (Int, Int) =
    issue match {
      case Issue(issueId, title, body, user, state) =>
        println(s"Creating issue $issueId")
        val previousIssueUrl = s"https://github.com/$srcRepo/issues/$issueId"

        val response = requests.post(
          s"https://api.github.com/repos/$destRepo/issues",
          data = ujson.Obj(
            "title" -> title,
            "body" -> s"$body\nID: $issueId\n Original Author: $user\nOriginal Issue: $previousIssueUrl"
          ),
          headers = Map("Authorization" -> s"token $token")
        )

        println(response.statusCode)
        issueId -> ujson.read(response)("number").num.toInt
    }

  private def closeIssue(
    issueId: Int,
    state: String,
    destRepo: String,
    token: String
  ) = {
    println(s"Closing issue $issueId")

    val response = requests.patch(
      s"https://api.github.com/repos/$destRepo/issues/$issueId",
      data = ujson.Obj("state" -> state),
      headers = Map("Authorization" -> s"token $token")
    )

    println(response.statusCode)
  }

  private def postComment(
    comment: Comment,
    newIssueId: Int,
    destRepo: String,
    token: String
  ): Unit = comment match {
    case Comment(issueId, user, body) => {
      println(s"Commenting on issue old_id=$issueId new_id=$newIssueId")

      val response = requests.post(
        s"https://api.github.com/repos/$destRepo/issues/$newIssueId/comments",
        data = ujson.Obj(
          "body" -> s"$body\nOriginal Author: $user"
        ),
        headers = Map("Authorization" -> s"token $token")
      )

      println(response.statusCode)
    }
  }

  private def fetchPaginated(
    url: String,
    token: String,
    page: Option[Int],
    responses: Option[List[ujson.Value]],
    params: (String, String)*
  ): List[ujson.Value] = {
    val _page = page.getOrElse(1)
    val _responses = responses.getOrElse(List.empty[ujson.Value])

    println(s"page ${_page}...")

    val resp = requests.get(
      url,
      params = Map("page" -> _page.toString) ++ params,
      headers = Map("Authorization" -> s"token $token")
    )

    val parsed = ujson.read(resp).arr

    if (parsed.length > 0) {
      _responses ++ fetchPaginated(
        url,
        token,
        Some(_page + 1),
        Some(parsed.toList),
        params: _*
      )
    } else {
      _responses
    }
  }

}
