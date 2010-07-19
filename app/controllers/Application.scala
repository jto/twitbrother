package controllers

import play._
import play.mvc._

import models._

object Application extends Controller {
    
    def index(user: String) = {
        val tweets = Tweet.getForUser(user)
        val tags = Tweet getTags(tweets) take 20
        render(tags)
    }
    
}
