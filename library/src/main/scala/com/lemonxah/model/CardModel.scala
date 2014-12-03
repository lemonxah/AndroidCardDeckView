package com.lemonxah.model

import android.view.View

/**
 * CardModel for use in the CardDeckAnimation
 * Created by lemonxah on 2014/09/16.
 */
case class CardModel[A](like: (A, View) => Unit, dislike: (A, View) => Unit, data: A) {
  require (like != null, "You have to specify a function to call when this card is liked")
  require (dislike != null, "You have to specify a function to call when this card is disliked")
  require (data != null, "You have to specify the data that will be passed into the like and dislike functions")
}
