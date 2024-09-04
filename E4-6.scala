enum Either[+E, +A]:
  case Left(value: E)
  case Right(value: A)

  def map[B](f: A => B): Either[E, B] = this match
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Right(a) => f(a)
    case Left(e) => Left(e)

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match
    case Right(a) => Right(a)
    case Left(_) => b

  def map2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C]
    = flatMap(a => that.map(b => f(a, b)))
