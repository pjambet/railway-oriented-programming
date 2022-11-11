
object RailwayList {

  sealed trait TwoTrack[F]

  case class Success[S](data: S) extends TwoTrack[S]

  case class Failure[S](messages: List[String]) extends TwoTrack[S]

  def succeed[S](x: S) = Success(x)

  def fail[S](message: String) = Failure[S](List(message))
  def fail[S](messages: List[String]) = Failure[S](messages)

  def bind[A, B](switchFunction: A => TwoTrack[B]): TwoTrack[A] => TwoTrack[B] = {
    { (twoTrackInput: TwoTrack[A]) =>
      twoTrackInput match {
        case Success(s) => switchFunction(s)
        case Failure(f) => fail(f)
      }
    }
  }

  def map[A, B](singleTrackFunction: A => B): TwoTrack[A] => TwoTrack[B] = { twoTrackInput: TwoTrack[A] =>
    twoTrackInput match {
      case Success(s) => succeed(singleTrackFunction(s))
      case Failure(f) => fail(f)
    }
  }

  def map2[A, B](singleTrackFunction: A => B): TwoTrack[A] => TwoTrack[B] = {
    bind(singleTrackFunction.andThen(succeed))
  }

  def tee[A](deadEndFunction: A => Unit)(a: A): A = {
    deadEndFunction(a)
    a
  }

  def tryCatch[A, B](f: A => B)(exnHandler: Throwable => String)(x: A): TwoTrack[B] = try {
    succeed(f(x))
  } catch {
    case ex: Throwable =>
      fail(exnHandler(ex))
  }

  def doubleMap[A, B](successFunc: A => B)
                     (failureFunc: List[String] => List[String])
                     (twoTrackInput: TwoTrack[A]): TwoTrack[B] = twoTrackInput match {
    case Success(s) => succeed(successFunc(s))
    case Failure(f) => fail(failureFunc(f))
  }

  def log[A](twoTrackInput: TwoTrack[A]): TwoTrack[A] = {
    val success = { x: A => println(s"DEBUG. Success so far: $x"); x }
    val failure = { xs: List[String] => println(s"ERROR. ${ xs.mkString("; ") }"); xs }
    doubleMap(success)(failure)(twoTrackInput)
  }

  def plus[A, B](addSuccess: (B, B) => B,
                 addFailure: (List[String], List[String]) => List[String],
                 switch1: A => TwoTrack[B],
                 switch2: A => TwoTrack[B])
                (x: A): TwoTrack[B] = {
    (switch1(x), switch2(x)) match {
      case (Success(s1), Success(s2)) => Success(addSuccess(s1, s2))
      case (Failure(f1), Success(_)) => Failure(f1)
      case (Success(_), Failure(f2)) => Failure(f2)
      case (Failure(f1), Failure(f2)) => Failure(addFailure(f1, f2))
    }
  }

  // ---

  case class Request(name: String, email: String)

  def nameNotBlank: Request => TwoTrack[Request] = { request: Request =>
    if (request.name == "") {
      fail("Name must not be blank")
    } else {
      succeed(request)
    }
  }


  def name50: Request => TwoTrack[Request] = { request: Request =>
    if (request.name.length > 50) {
      fail("Name must not be longer than 50 chars")
    } else {
      succeed(request)
    }
  }

  def emailNotBlank: Request => TwoTrack[Request] = { request: Request =>
    if (request.email == "") {
      fail("Email must not be blank")
    } else {
      succeed(request)
    }
  }

  sealed case class ComposableSwitch[A, B](f1: A => TwoTrack[B]) {
    def &&&(f2: A => TwoTrack[B]): A => TwoTrack[B] = {
      val addSuccess: (B, B) => B = (r1: B, _: B) => r1
      val addFailure: (List[String], List[String]) => List[String] = (s1: List[String], s2: List[String]) => s1 ++ s2
      plus(addSuccess, addFailure, f1, f2)
    }
  }

  implicit def functionToComposableFunction[A, B](f: A => TwoTrack[B]) = ComposableSwitch(f)

  def validateRequest: Request => TwoTrack[Request] =
    nameNotBlank &&& name50 &&& emailNotBlank

  def validateRequest_inlined(input: Request): TwoTrack[Request] = {

    val nameNotBlankResult = if (input.name == "") {
      fail("Name must not be blank")
    } else {
      succeed(input)
    }

    nameNotBlankResult match {
      case Success(s) => {
        val name50Result = if (s.name.length > 50) {
          fail("Name must not be longer than 50 chars")
        } else {
          succeed(s)
        }

        name50Result match {
          case Success(s) => {
            if (s.email == "") {
              fail("Email must not be blank")
            } else {
              succeed(s)
            }
          }
          case Failure(f) => fail(f)
        }
      }
      case Failure(f) => fail(f)
    }

  }

  def updateDB(request: Request): Unit = {
    throw new RuntimeException("Fake DB Error")
    ()
  }

  val updateDBStep: Request => TwoTrack[Request] =
    tryCatch(tee(updateDB))(ex => ex.getMessage)


  def canonicalizeEmail(request: Request): Request = {
    request.copy(email = request.email.trim().toLowerCase())
  }

  def main(args: Array[String]): Unit = {

    Tuple22.apply()

    val railway =
      validateRequest
        .andThen(map2(canonicalizeEmail))
        .andThen(bind(updateDBStep))
        .andThen(log)

    val request = Request(name = "", email = "")

    railway(request)
  }
}

