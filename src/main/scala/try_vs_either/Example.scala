package try_vs_either

import try_vs_either.EitherExample.ServiceUnavailable

import scala.util.{Random, Failure, Success, Try}
import scalaz.\/

case class TaskList(title: String)
case class User(id: Long)

/**
 * Ideen:
 * Erst Either zeigen. Alles toll. Dann Try.
 *
 * - Try is doch nützlich weil userId ein String ist und eine NumberFormatException wirft.
 * - Either bieter kein recover? Kein teilweises recover von einigen Fehlern möglich.
 *
 *
 */

import EitherExample.MyError

trait EitherTodoService {
  def authenticate(userId: String, secret: String): \/[MyError, User]
  def fetchLists(user: User): \/[MyError, Seq[TaskList]]
}
trait TryTodoService {
  def authenticate(userId: String, secret: String): Try[User]
  def listsForUser(userId: String, secret: String): Try[Seq[TaskList]]
}

object EitherExample {
  import scalaz._

  sealed trait MyError
  case class UnknownUser(userId: Long) extends MyError {
    override def toString = s"User with id [$userId] is unknown."
  }
  case class WrongSecret(userId: Long) extends MyError {
    override def toString = s"User with id [$userId] provided the wrong secret."
  }
  case class ServiceUnavailable(service: String) extends MyError {
    override def toString = s"The Service [$service] is currently unavailable"
  }

  def authenticate(userId: String, secret: String): \/[MyError, User] = userId.toLong match {
    case u @ 999  => \/ left UnknownUser(u)
    case u @ 1000 => \/ left WrongSecret(u)
    case u        => \/ right User(u)
  }
  def fetchLists(user: User): \/[MyError, Seq[TaskList]] = {
    if(Random.nextInt(100) <= 80){
      \/.right(Seq(TaskList("Inbox"), TaskList("Private"), TaskList("Work")))
    } else {
      \/.left(ServiceUnavailable("ListService"))
    }
  }
  val service: EitherTodoService = ???
  def listsForUser(userId: String, secret: String): \/[MyError, Seq[TaskList]] = {
    val listsForUser: \/[MyError, Seq[TaskList]] = for {
      user  <- service.authenticate(userId, secret)
      lists <- service.fetchLists(user)
    } yield lists

    listsForUser match {
      case lists @ \/-(_) =>
        lists
      case -\/(_ : ServiceUnavailable) =>
        \/.right(Seq.empty[TaskList])
      case error @ -\/(_) =>
        error
    }
  }
}

object TryExample {
  sealed class MyException(msg: String) extends Exception(msg, null)
  case class UnknownUserException(userId: Long) extends MyException(s"User with id [$userId] is unknown.")
  case class WrongSecretException(userId: Long) extends MyException(s"User with id [$userId] provided the wrong secret.")
  case class ServiceUnavailableException(service: String) extends MyException(s"The Service [$service] is currently unavailable")

  def authenticate(userId: String, secret: String): Try[User] = Try {
    userId.toLong match {
      case u @ 999  => throw new UnknownUserException(u)
      case u @ 1000 => throw new WrongSecretException(u)
      case u        => User(u)
    }
  }

  @throws[ServiceUnavailableException]("if the service is not available")
  def fetchLists(user: User): Try[Seq[TaskList]] = Try {
    if(Random.nextInt(100) <= 80){
      Seq(TaskList("Inbox"), TaskList("Private"), TaskList("Work"))
    } else {
      throw new ServiceUnavailableException("ListService")
    }
  }

  def listsForUser(userId: String, secret: String): Try[Seq[TaskList]] = {
    val listsForUser: Try[Seq[TaskList]] = for {
      user  <- authenticate(userId, secret)
      lists <- fetchLists(user)
    } yield lists

    listsForUser.recover {
      case e: ServiceUnavailableException => Seq.empty[TaskList]
    }
  }
}
