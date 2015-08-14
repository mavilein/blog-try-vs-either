package try_vs_either

import scala.util.{Random, Failure, Success, Try}

case class WList(title: String)
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

object EitherExample {
  import scalaz._

  sealed class MyError
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
    case u @ 1000 => \/ left UnknownUser(u)
    case u        => \/ right User(u)
  }
  def fetchLists(user: User): \/[MyError, Seq[WList]] = {
    if(Random.nextInt(100) <= 80){
      \/.right(Seq(WList("Inbox"), WList("Private"), WList("Work")))
    } else {
      \/.left(ServiceUnavailable("ListService"))
    }
  }

  def listsForUser(userId: String, secret: String): \/[MyError, Seq[WList]] = {
    val listsForUser: \/[MyError, Seq[WList]] = for {
      user  <- authenticate(userId, secret)
      lists <- fetchLists(user)
    } yield lists

    listsForUser match {
      case lists @ \/-(_) =>
        lists
      case -\/(_ : ServiceUnavailable) =>
        \/.right(Seq.empty[WList])
      case error @ -\/(_) =>
        error
    }
  }
}

object TryExample {
  sealed class MyException(msg: String, e: Exception) extends Exception(msg, e)
  case class NumberFormatException(msg: String, e: Exception) extends MyException(msg, e)
  case class ServiceUnavailableException(msg: String) extends MyException(msg, null)

  // könnte NumberFormatException werfen
  def authenticate(userId: Long, secret: String): Try[User] = ???
  // eventuell eine ServiceUnavailableException
  def fetchLists(user: User): Try[Seq[WList]] = ???

  val listsForUser: Try[Seq[WList]] = for {
    user  <- authenticate(123, "my-secret")
    lists <- fetchLists(user)
  } yield lists

  listsForUser match {
    case Success(lists) =>
      println(s"Success! We got the following lists: $lists")
    case Failure(exception) =>
      println(s"Failure! We got the following exception: $exception")
  }
  // handle error
  listsForUser.recover {
    case e: NumberFormatException =>
    case e: ServiceUnavailableException =>
  }
}
