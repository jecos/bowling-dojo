sealed trait User

case class LoggedUser(username: String) extends User

case class AnonymousUser(token: String) extends User

val user1: User = LoggedUser("Maurice")
val user2: User = AnonymousUser("token")

user1 match {
  case LoggedUser(myName) => s"Welcome back ${myName}"
  case AnonymousUser(_) => "Sign in, dude!"
}