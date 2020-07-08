
object Railway {

  sealed trait TwoTrack[F]

  case class Success[S](data: S) extends TwoTrack[S]
  case class Failure[S](message: String) extends TwoTrack[S]


  def bind[A, B](switchFunction: A => TwoTrack[B])(twoTrackInput: TwoTrack[A]): TwoTrack[B] = {
    twoTrackInput match {
      case Success(s) => switchFunction(s)
      case Failure(f) => Failure(f)
    }
  }

  def bind2[A, B](switchFunction: A => TwoTrack[B]): TwoTrack[A] => TwoTrack[B] = {
    { (twoTrackInput: TwoTrack[A]) =>
      twoTrackInput match {
        case Success(s) => switchFunction(s)
        case Failure(f) => Failure(f)
      }
    }
  }
  //  def unit[A]: A => TwoTrack[A] = Right.apply
  //
  //  def switch[A, B](f: A => B): A => TwoTrack[B] = f.andThen(Right.apply)

  def map[A, B](singleTrackFunction: A => B): TwoTrack[A] => TwoTrack[B] = { twoTrackInput: TwoTrack[A] =>
    twoTrackInput match {
      case Success(s) => Success(singleTrackFunction(s))
      case Failure(f) => Failure(f)
    }
  }

  def map2[A, B](singleTrackFunction: A => B): TwoTrack[A] => TwoTrack[B] = {
    bind(singleTrackFunction.andThen(Success.apply))
  }

  def tee[A](deadEndFunction: A => Unit)(a: A): A = {
    deadEndFunction(a)
    a
  }

  def tryCatch[A, B](f: A => B)(exnHandler: Throwable => String)(x: A): TwoTrack[B] = try {
    Success(f(x))
  } catch {
    case ex: Throwable =>
      Failure(exnHandler(ex))
  }

  def doubleMap[A, B](successFunc: A => B)
                     (failureFunc: String => String)
                     (twoTrackInput: TwoTrack[A]): TwoTrack[B] = twoTrackInput match {
    case Success(s) => Success(successFunc(s))
    case Failure(f) => Failure(failureFunc(f))
  }

  def log[A](twoTrackInput: TwoTrack[A]): TwoTrack[A] = {
    val success = { x: A => println(s"DEBUG. Success so far: $x"); x }
    val failure = { x: String => println(s"ERROR. $x"); x }
    doubleMap(success)(failure)(twoTrackInput)
  }

  // ---

  case class Request(name: String, email: String)

  def nameNotBlank(request: Request): TwoTrack[Request] =
    if (request.name == "") {
      Failure("Name must not be blank")
    } else {
      Success(request)
    }


  def name50(request: Request): TwoTrack[Request] =
    if (request.name.length > 50) {
      Failure("Name must not be longer than 50 chars")
    } else {
      Success(request)
    }


  def emailNotBlank(request: Request): TwoTrack[Request] =
    if (request.email == "") {
      Failure("Email must not be blank")
    } else {
      Success(request)
    }


  def validateRequest(twoTrackInput: TwoTrack[Request]): TwoTrack[Request] = {
    println("validateRequest")
    bind2(nameNotBlank)
      .andThen(bind(name50))
      .andThen(bind(emailNotBlank))(twoTrackInput)
  }

  def updateDB(request: Request): Unit = {
    println("DB STEP")
    throw new RuntimeException("YO YO YO")
    ()
  }

  val updateDBStep: Request => TwoTrack[Request] =
    tryCatch(tee(updateDB))(ex => ex.getMessage)


  def canonicalizeEmail(request: Request): Request = {
    println("canonicalizeEmail")
    request.copy(email = request.email.trim().toLowerCase())
  }

  def main(args: Array[String]): Unit = {

    //    unit[Request]
    //      .andThen(validateRequest)
    //      .andThen(map_(canonicalizeEmail))
    //      .andThen(map_(tee(updateDBFromRequest)))
    //      .apply(Request("a", "b"))
    //
    //    println("---")
    //
    //    validateRequest_either(Request("a", "b"))
    //      .flatMap(switch(canonicalizeEmail))
    //
    //    for {
    //      a <- validateRequest_either(Request("a", "b"))
    //      _ <- switch(canonicalizeEmail)(a)
    //    } yield ()

    val f1 = tee(updateDB) _
    val f = (validateRequest _)
      .andThen(map(canonicalizeEmail))
      .andThen(bind(updateDBStep))
      .andThen(log)

    f.apply(Success(Request(name = "", email = "")))

    println("---")

    println(updateDBStep(Request("name", "email")))

    println("Done!")
  }
}

