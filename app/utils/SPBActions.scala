package utils

import cake.GlobalCake
import play.api.mvc._
import scala.concurrent.Future
import services.AuthServiceComponent
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Action builder that supports currying and composition of Play actions
 */
object SPBActions extends SPBActionsImpl[AnyContent, Request[AnyContent], Future[Result]](CurriedActionBuilder) {
  def apply[A](bp: BodyParser[A]) = SPBActionsImpl(CurriedActionBuilder(bp))
}

case class SPBActionsImpl[BodyType, InputType, OutputType](
  cab: CurriedActionBuilder[Request, BodyType, InputType, OutputType],
  dependencies: AuthServiceComponent = GlobalCake) extends Results {

  def async(block: (InputType) => OutputType): Action[BodyType] = cab.async(block)

  def disabled(block: (InputType) => OutputType): Action[BodyType] = throw new NotImplementedError

  private def handleErrors = constructAction { next => implicit request =>
    // Wrap any exceptions in a Future
    val nextRequest = try {
      next(request)
    } catch {
      case e: Throwable => Future.failed(e)
    }

    nextRequest.recover {
      // If not an internal server error, then don't go through Play's default onError handling which would throw
      // an error and create a 500 response
      case e: SPBException if e.status != Results.InternalServerError =>
        SPBException.buildResponse(e)
      case e => throw e
    }
  }

  /**
   * Check if the user is logged in and has access to LED (but do not redirect them to login page)
   * NB: actions are processed from right to left due to currying, but the Future is built up from left to right
   *      Therefore, .handleErrors must always be last. See the CurriedActionBuilder docs.
   */
  def apiAction = handleErrors


  // Use in other actions to reduce boilerplate (without currying a value)
  private def constructAction(
    f: (Request[BodyType] => Future[Result]) => Request[BodyType] => Future[Result]
  ): SPBActionsImpl[BodyType, InputType, OutputType] = {
    copy(cab.compose(ComposeFunction.forRequest[BodyType](f)))
  }

  // Use in other actions to reduce boilerplate (with currying a value)
  private def constructCurriedAction[CurryType](
    f: ((Request[BodyType], CurryType) => Future[Result]) => Request[BodyType] => Future[Result]
  ): SPBActionsImpl[BodyType, CurryType, InputType => OutputType] = {
    copy(cab.curryCompose(CurryComposeFunction.forRequest[BodyType, CurryType](f)))
  }
}