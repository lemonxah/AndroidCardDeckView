package com.lemonxah.adapter

import android.content.Context
import android.view.{ViewGroup, View}
import android.widget.BaseAdapter
import com.lemonxah.model.CardModel

import scala.collection.mutable


/**
 * CardDeck Adapter for the CardDeckView
 * Created by lemonxah on 2014/09/16.
 */
abstract class CardDeckAdapter[A] extends BaseAdapter {
  val data = new mutable.Queue[CardModel[A]]()
  override def getCount: Int = data.length
  override def getItemId(position: Int): Long = data(position).hashCode()

  override def getView(position: Int, convertView: View, parent: ViewGroup): View =
    bindCardView(getItem(position), getCardView(convertView, parent))

  def getCardView(convertView: View, parent: ViewGroup): View
  def bindCardView(model: CardModel[A], view: View) : View
  override def getItem(position: Int): CardModel[A] = data(position)

  def add(cardModels: Array[CardModel[A]]): Unit = {
    data ++= cardModels
    notifyDataSetChanged()
  }

  def pop: CardModel[A] = {
    data.dequeue()
  }
}
