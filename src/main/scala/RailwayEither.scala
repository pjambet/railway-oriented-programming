import scala.util.{Failure, Success, Try}

object RailwayEither {

  type TwoTrack[S] = Either[String, S]

  def fail[S](message: String) = Left(message)

  def succeed[S](x: S) = Right(x)

  def switch[A, B](fn: A => B): A => TwoTrack[B] =
    fn.andThen(Right.apply)

  def tryCatch[A](fn: A => Unit)(x: A): Either[String, A] = {
    Try(fn(x)) match {
      case Failure(exception) => Left(exception.getMessage)
      case Success(_) => Right(x)
    }
  }

  case class Request(name: String, email: String)

  def nameNotBlank(request: Request): TwoTrack[Request] =
    if (request.name == "") {
      fail("Name must not be blank")
    } else {
      succeed(request)
    }


  def name50(request: Request): TwoTrack[Request] =
    if (request.name.length > 50) {
      fail("Name must not be longer than 50 chars")
    } else {
      succeed(request)
    }


  def emailNotBlank(request: Request): TwoTrack[Request] =
    if (request.email == "") {
      fail("Email must not be blank")
    } else {
      succeed(request)
    }


  def validateRequest(twoTrackInput: TwoTrack[Request]): TwoTrack[Request] = {
    for {
      r <- twoTrackInput
      r <- nameNotBlank(r)
      r <- name50(r)
      r <- emailNotBlank(r)
    } yield r
  }

  def updateDB(request: Request): Unit = {
    //    throw new RuntimeException("Fake DB Error")
    ()
  }

  def canonicalizeEmail(request: Request): Request = {
    request.copy(email = request.email.trim().toLowerCase())
  }

  def logSuccess[A](x: A): TwoTrack[A] = {
    println(s"DEBUG. Success so far: $x");
    succeed(x)
  }

  def logFailure[A](x: String): TwoTrack[A] = {
    println(s"ERROR. $x");
    fail(x)
  }

  def main(args: Array[String]): Unit = {

    val request = Request(name = "Pierre", email = "pierre@pjam.me")

    val updateDBStep: Request => TwoTrack[Request] = tryCatch(updateDB)

    val railway = validateRequest(succeed(request))
      .flatMap(switch(canonicalizeEmail))
      .flatMap(updateDBStep)
      .fold(logFailure, logSuccess)

    println(railway)
  }
}

