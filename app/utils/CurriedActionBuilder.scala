package utils

import play.api.mvc.{Action, AnyContent, Result, Request, BodyParsers, BodyParser}
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent.Future
import language.higherKinds

abstract class CurriedActionBuilder[R[_], BodyType, InputType, OutputType](bodyParser: BodyParser[BodyType]) { self =>

  def async(block: InputType => OutputType): Action[BodyType] = new Action[BodyType] {
    override def apply(request: Request[BodyType]): Future[Result] = {
      toInternalRequest(request).flatMap { internalRequest =>
        run(block, internalRequest)
      }
    }
    override val parser: BodyParser[BodyType] = bodyParser
  }

  protected def toInternalRequest(r: Request[BodyType]): Future[R[BodyType]]

  def curryCompose[NewInputType](curryComposeFunction: CurryComposeFunction[R, BodyType, NewInputType]): CurriedActionBuilder[R, BodyType, NewInputType, InputType => OutputType] =
    new CurriedActionBuilder[R, BodyType, NewInputType, InputType => OutputType](bodyParser) {
      override protected def toInternalRequest(r: Request[BodyType]): Future[R[BodyType]] = self.toInternalRequest(r)
      override protected[utils] def run(f: NewInputType => InputType => OutputType, request: R[BodyType]): Future[Result] = {
        curryComposeFunction((updatedRequest, newInput) =>
          self.run(f(newInput), updatedRequest)
        )(request)
      }
    }

  def compose(composeFunction: ComposeFunction[R, BodyType]): CurriedActionBuilder[R, BodyType, InputType, OutputType] =
    new CurriedActionBuilder[R, BodyType, InputType, OutputType](bodyParser) {
      override protected def toInternalRequest(r: Request[BodyType]): Future[R[BodyType]] = self.toInternalRequest(r)

      override protected[utils] def run(f: InputType => OutputType,
                                          request: R[BodyType]): Future[Result] = {
        composeFunction(updatedRequest => self.run(f, updatedRequest))(request)
      }
    }

  def composeWrappedAction(actionMaker: Action[BodyType] => Action[BodyType])
                          (implicit evFromRequest: Request[BodyType] =:= R[BodyType],
                           evToRequest: R[BodyType] =:= Request[BodyType]): CurriedActionBuilder[R, BodyType, InputType, OutputType] = {
    compose {
      new ComposeFunction[R, BodyType] {
        override def apply(next: (R[BodyType]) => Future[Result])
                          (implicit request: R[BodyType]): Future[Result] = {
          val nextAsAction = new Action[BodyType] {
            override def apply(request: Request[BodyType]): Future[Result] = {
              next(evFromRequest(request))
            }
            override val parser: BodyParser[BodyType] = bodyParser
          }
          actionMaker(nextAsAction)(evToRequest(request))
        }
      }
    }
  }

  protected[utils] def run(f: InputType => OutputType, request: R[BodyType]): Future[Result]
}

abstract class StartingCurriedActionBuilder[R[_], BodyType](bodyParser: BodyParser[BodyType])
  extends CurriedActionBuilder[R, BodyType, R[BodyType], Future[Result]](bodyParser) {
  override protected[utils] def run(f: R[BodyType] => Future[Result], request: R[BodyType]): Future[Result] = {
    f(request)
  }
}

object CurriedActionBuilder extends StartingCurriedActionBuilder[Request, AnyContent](BodyParsers.parse.anyContent) {
  override protected def toInternalRequest(r: Request[AnyContent]): Future[Request[AnyContent]] =
    Future.successful(r)

  def apply[BodyType](bodyParser: BodyParser[BodyType]): CurriedActionBuilder[Request, BodyType, Request[BodyType], Future[Result]] =
    new StartingCurriedActionBuilder[Request, BodyType](bodyParser) {
      override protected def toInternalRequest(r: Request[BodyType]): Future[Request[BodyType]] =
        Future.successful(r)
    }
}

trait ComposeFunction[R[_], BodyType] {
  def apply(next: R[BodyType] => Future[Result])(implicit request: R[BodyType]): Future[Result]
}

object ComposeFunction {

  def forRequest[BodyType](block: (Request[BodyType] => Future[Result]) => Request[BodyType] => Future[Result]): ComposeFunction[Request, BodyType] = {
    new ComposeFunction[Request, BodyType] {
      def apply(next: Request[BodyType] => Future[Result])
               (implicit request: Request[BodyType]): Future[Result] = {
        block(next)(request)
      }
    }
  }
}

trait CurryComposeFunction[R[_], BodyType, NewInputType] { self =>
  def apply(next: (R[BodyType], NewInputType) => Future[Result])
           (implicit request: R[BodyType]): Future[Result]

  def toComposeFunction: ComposeFunction[R, BodyType] = {
    new ComposeFunction[R, BodyType] {
      override def apply(next: (R[BodyType]) => Future[Result])
                        (implicit request: R[BodyType]): Future[Result] = {
        self.apply((r, _) => next(r))
      }
    }
  }
}

object CurryComposeFunction {
  def forRequest[BodyType, NewInputType](block: ((Request[BodyType], NewInputType) => Future[Result]) => Request[BodyType] => Future[Result]):CurryComposeFunction[Request, BodyType, NewInputType] = {
    new CurryComposeFunction[Request, BodyType, NewInputType] {
      def apply(next: (Request[BodyType], NewInputType) => Future[Result])
               (implicit request: Request[BodyType]): Future[Result] = {
        block(next)(request)
      }
    }
  }
}
