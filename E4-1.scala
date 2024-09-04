enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case Some(a) => Some(f(a))
    case None => None

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match
    case Some(a) => a
    case None => default

  def orElse[B >: A](ob: => Option[B]): Option[B] = ob.map(_ => this).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = flatMap(a => if f(a) then this else None)
