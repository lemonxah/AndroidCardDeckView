package com.lemonxah.decktest

import android.app.Activity
import android.content.DialogInterface
import android.content.DialogInterface.OnClickListener
import android.os.Bundle
import android.view._
import android.widget.{Button, Toast, TextView}
import com.lemonxah.adapter.CardDeckAdapter
import com.lemonxah.model.CardModel
import com.lemonxah.view.CardDeckView
import com.lemonxah.view.SwipeDirection._

/**
 * scala exmaple
 * Created by lemonxah on 2014/10/02.
 */
case class SomeData(Title: String, Description: String)
class MyScalaAdapter(context: Activity) extends CardDeckAdapter[SomeData] {

  def getLayoutInflater: LayoutInflater = {
    context.getLayoutInflater
  }

  def getCardView(convertView: View, parent: ViewGroup): View = {
    if (convertView == null) getLayoutInflater.inflate(R.layout.card, null)
    else convertView
  }

  def bindCardView(model: CardModel[SomeData], view: View): View = {
    val t: TextView = view.findViewById(R.id.test_id).asInstanceOf[TextView]
    t.setText(model.data.Title)
    view
  }
}

class MainActivity extends Activity {
  implicit def funtoclick(f: View => Any) = new View.OnClickListener {
    override def onClick(v: View): Unit = f(v)
  }
  var adapter: MyScalaAdapter = _
  protected override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.activity_my)
    val cdv: CardDeckView[SomeData] = findViewById(R.id.card_deck).asInstanceOf[CardDeckView[SomeData]]
    val lb = findViewById(R.id.left_button).asInstanceOf[Button]
    val rb = findViewById(R.id.right_button).asInstanceOf[Button]
    if (adapter == null) {
      adapter = new MyScalaAdapter(this)
    }

    lb.setOnClickListener((v:View) => {
      cdv.doDislike()
    })
    rb.setOnClickListener((v:View) => {
      cdv.doLike()
    })

    cdv.setAdapter(adapter)
    cdv.setLowDataTrigger(() => adapter.add(getCardModels(10)))
    cdv.setOnClick((s:SomeData, v:View) => { Toast.makeText(this, s"Title:${s.Title}",Toast.LENGTH_SHORT).show() })
    cdv.setOnSwipeUpdate((direction, percentage, view) => {
      val v = direction match {
        case SwipeLeft =>
          view.findViewById(R.id.invite).setVisibility(View.GONE)
          view.findViewById(R.id.skip)
        case SwipeRight =>
          view.findViewById(R.id.skip).setVisibility(View.GONE)
          view.findViewById(R.id.invite)
      }
      percentage match {
        case 0 =>
          v.setVisibility(View.GONE)
        case p =>
          v.setVisibility(View.VISIBLE)
          v.setAlpha(p)
      }
    })

  }

  private def getCardModels(count: Int): Array[CardModel[SomeData]] = {
    (0 until count toArray).map(i => {
      CardModel((sd:SomeData, v:View) => {
        v.findViewById(R.id.invite).setVisibility(View.VISIBLE)
        v.findViewById(R.id.invite).setAlpha(1F)
      }, (sd:SomeData, v:View) => {
        v.findViewById(R.id.skip).setAlpha(1F)
        v.findViewById(R.id.skip).setVisibility(View.VISIBLE)
      }, SomeData(i.toString,"SomeDesc"))
    })
  }

  override def onCreateOptionsMenu(menu: Menu): Boolean = {
    getMenuInflater.inflate(R.menu.my, menu)
    true
  }

  override def onOptionsItemSelected(item: MenuItem): Boolean = {
    val id: Int = item.getItemId
    if (id == R.id.action_settings) {
      return true
    }
    super.onOptionsItemSelected(item)
  }
}

