package try_vs_either

import try_vs_either.EitherExample.{WrongSecret, UnknownUser, ServiceUnavailable}

import scala.util.{Random, Failure, Success, Try}
import scalaz.\/

case class Page(title: String, content: String)
case class User(id: Long)

object DefaultHomePage extends Page("Welcome!", "This is your amazing Homepage!")

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

trait EitherService {
  def authenticate(userId: String, secret: String): \/[MyError, User]
  def fetchHomePage(user: User): \/[MyError, Page]
}
trait TryService {
  def authenticate(userId: String, secret: String): Try[User]
  def fetchHomePage(user: User): Try[Page]
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

  object EitherService extends EitherService {
    def authenticate(userId: String, secret: String): \/[MyError, User] = userId.toLong match {
      case u @ 999  => \/ left UnknownUser(u)
      case u @ 1000 => \/ left WrongSecret(u)
      case u        => \/ right User(u)
    }
    def fetchHomePage(user: User): \/[MyError, Page] = {
      if(Random.nextInt(100) <= 80){
        \/.right(Page("Your Homepage", "Welcome to your Homepage!"))
      } else {
        \/.left(ServiceUnavailable("HomePageService"))
      }
    }
  }


  val service: EitherService = EitherService
  def homePageForUser(userId: String, secret: String): \/[MyError, Page] = {
    val homepage: \/[MyError, Page] = for {
      user     <- service.authenticate(userId, secret)
      homePage <- service.fetchHomePage(user)
    } yield homePage

    homepage match {
      case \/-(_) =>
        homepage
      case -\/(_ : ServiceUnavailable) =>
        \/.right(DefaultHomePage)
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

  object TryService extends TryService {
    def authenticate(userId: String, secret: String): Try[User] = Try {
      userId.toLong match {
        case u @ 999  => throw new UnknownUserException(u)
        case u @ 1000 => throw new WrongSecretException(u)
        case u        => User(u)
      }
    }

    @throws[ServiceUnavailableException]("if the service is not available")
    def fetchHomePage(user: User): Try[Page] = Try {
      if(Random.nextInt(100) <= 80){
        Page("Your Homepage", "Welcome to your Homepage!")
      } else {
        throw new ServiceUnavailableException("HomePageService")
      }
    }
  }

  val service: TryService = TryService

  def listsForUser(userId: String, secret: String): Try[Page] = {
    val homePageForUser: Try[Page] = for {
      user     <- service.authenticate(userId, secret)
      homePage <- service.fetchHomePage(user)
    } yield homePage

    homePageForUser.recover {
      case e: ServiceUnavailableException => DefaultHomePage
    }
  }
}
