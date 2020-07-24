package controllers

import javax.inject._
import play.api._
import play.api.mvc._

/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class HomeController @Inject() (val controllerComponents: ControllerComponents)
    extends BaseController {

  /**
    * Create an Action to render an HTML page.
    *
    * The configuration in the `routes` file means that this method
    * will be called when the application receives a `GET` request with
    * a path of `/`.
    */
  def index(paramState: Option[String], paramX: Option[Int], paramY: Option[Int]) =
    Action { implicit request: Request[AnyContent] =>
      val state = icfp2020.V.demodulate(paramState getOrElse "00")
      val x = paramX getOrElse 0
      val y = paramY getOrElse 0
      Ok(views.html.index())
    }
}
