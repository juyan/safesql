package services

/**
  * Created by junyan on 3/26/16.
  */
trait AuthServiceComponent {

  trait AuthService {
    def foo(): Int
  }

  val authService: AuthService
}

trait DefaultAuthServiceComponent extends AuthServiceComponent {
  override val authService = new AuthService {
    override def foo() = 1
  }
}
