package com.michaelmaysonet74.handsonscala.chapters

import requests._
import upickle._

object Chapter12 {
  import Chapter3.withFileReader

  private def getToken(fileName: String): String =
    withFileReader(fileName) { reader =>
      reader.readLine
    }.getOrElse("Failed")

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

  private def getIssues(srcRepo: String, token: String) = {
    val issues = fetchPaginated(
      s"https://api.github.com/repos/$srcRepo/issues",
      token,
      None,
      None,
      "state" -> "all"
    )

    issues
      .filter(!_.obj.contains("pull_request"))
      .map { issue =>
        (
          issue("number").num.toInt,
          issue("title").str,
          issue("body").str,
          issue("user")("login").str,
          issue("state").str
        )
      }
      .sortBy((number) => number)
      .toList
  }

  private def postIssue(
    issue: (Int, String, String, String, String),
    srcRepo: String,
    destRepo: String,
    token: String
  ): (Int, Int) =
    issue match {
      case (issueId, title, body, user, state) => {
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

        (
          issueId,
          ujson.read(response)("number").num.toInt
        )
      }
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

  private def getComments(srcRepo: String, token: String) = {
    val comments = fetchPaginated(
      s"https://api.github.com/repos/$srcRepo/issues/comments",
      token,
      None,
      None
    )

    comments.map { comment =>
      (
        comment("issue_url").str match {
          case s"https://api.github.com/repos/$srcRepo/issues/$id" => id.toInt
        },
        comment("user")("login").str,
        comment("body").str
      )
    }.toList
  }

  private def postComment(
    comment: (Int, String, String),
    newIssueId: Int,
    destRepo: String,
    token: String
  ) = {
    comment match {
      case (issueId, user, body) => {
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
  }

  private def migrateIssuesWithComments(
    srcRepo: String,
    destRepo: String,
    token: String
  ) = {
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
      .filter(issue => issue._5 == "closed" && issueNumMap.get(issue._1) != None)
      .map(issue =>
        closeIssue(
          issueNumMap(issue._1).toInt,
          issue._5,
          destRepo,
          token
        )
      )

    comments.filter(comment => issueNumMap.get(comment._1) != None).map { comment =>
      postComment(
        comment,
        issueNumMap(comment._1).toInt,
        destRepo,
        token
      )
    }
  }

  def execute(): Unit = {
    val srcRepo = "lihaoyi/requests-scala"
    val destRepo = "michaelmaysonet74/issue-migration"

    getToken(
      "/Users/michael/code/hands-on-scala/github_token.txt"
    ) match {
      case "Failed" => println("Couldn't get token.")
      case token =>
        migrateIssuesWithComments(
          srcRepo,
          destRepo,
          token
        )
    }
  }
}
