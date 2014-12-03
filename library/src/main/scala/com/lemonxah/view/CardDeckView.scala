package com.lemonxah.view

import android.animation._
import android.content.Context
import android.content.res.TypedArray
import android.database.DataSetObserver
import android.graphics.{Matrix, Rect}
import android.util.{Log, AttributeSet}
import android.view.GestureDetector.SimpleOnGestureListener
import android.view.View.MeasureSpec
import android.view.ViewGroup.LayoutParams
import android.view._
import android.view.animation.{AccelerateInterpolator, LinearInterpolator}
import android.widget.AdapterView
import com.lemonxah.adapter.CardDeckAdapter
import com.lemonxah.carddeckview.R
import com.lemonxah.model.{CardModel, Orientation}
import com.lemonxah.model.Orientation._
import com.lemonxah.view.AnimateDirection._
import com.lemonxah.view.SwipeDirection._

import scala.util.Random

/**
 * CardDeckView and companion object
 * Created by lemonxah on 2014/09/12.
 */
object CardDeckView {
  var mTouchSlop: Int = _
  var mFlingSlop: Int = _
  var mAnimating: Boolean = false
  def TopCard[A](cdv: CardDeckView[A]): Option[View] = Option(if (cdv.getChildCount > 0) cdv.getChildAt(cdv.getChildCount - 1) else null)
  private val boundsRect = new Rect()
  def AnimationListener[A](cdv: CardDeckView[A],v: View) = new AnimatorListenerAdapter() {
    override def onAnimationCancel(animation: Animator): Unit = { }
    override def onAnimationEnd(animation: Animator): Unit = {
      cdv.removeViewInLayout(v)
      cdv.ensureFull()
      mAnimating = false
    }
  }

  class LayoutParams(width: Int, height: Int) extends ViewGroup.LayoutParams(width, height) { }
  class GestureListener[A](cdv: CardDeckView[A]) extends SimpleOnGestureListener {
    override def onFling(e1: MotionEvent, e2: MotionEvent, velocityX: Float, velocityY: Float): Boolean = {
      (TopCard(cdv), mAnimating) match {
        case (_, true) => false
        case (None, _) => false
        case (Some(v), _) =>
          if (Math.abs(e2.getX - e1.getX) > mTouchSlop &&
              Math.abs(velocityX) > Math.abs(velocityY) &&
              math.abs(velocityX) > mFlingSlop * 5) {

            mAnimating = true
            var (targetX, targetY, duration: Long) = (v.getX, v.getY, 0L)

            boundsRect.set(0 - v.getWidth - 100,
              0 - v.getHeight - 100,
              cdv.getWidth + 100,
              cdv.getHeight + 100)

            while(boundsRect.contains(targetX.toInt, targetY.toInt)) {
              targetX = targetX + (velocityX / 10)
              targetY = targetY + (velocityY / 10)
              duration = duration + 100
            }

            duration = Math.min(500, duration)
            val cardModel = cdv.getAdapter.pop
            if (targetX > 0) cardModel.like(cardModel.data, v)
            else cardModel.dislike(cardModel.data, v)

            v.animate()
              .setDuration(duration)
              .alpha(.75f)
              .setInterpolator(new LinearInterpolator())
              .x(targetX)
              .y(targetY)
              .rotation(Math.copySign(45, velocityX))
              .setListener(AnimationListener(cdv,v))
            true
          } else false
      }
    }
  }
}

class CardDeckView[A](context: Context, attrs: AttributeSet, defStyle: Int)
  extends AdapterView[CardDeckAdapter[A]](context,attrs, defStyle) {
  def this(context: Context, attrs: AttributeSet) = this(context, attrs, 0)
  def this(context: Context) = this(context, null)

  implicit def hex2int (hex: String): Int = Integer.parseInt(hex, 16)

  private var mAdapter: CardDeckAdapter[A] = _
  private final val ROTATION_RADIANS: Double = Math.PI / 256
  private var mOrientation = Disordered
  private var mGravity: Int = Gravity.CENTER
  private lazy val mLowDataLimit: Int = mMaxVisible * 2
  private val boundsRect = new Rect()

  private val childRect = new Rect()
  private val mMatrix = new Matrix()
  private var mGestureDetector: GestureDetector = _

  private var mUseFling: Boolean = true
  private var mMaxVisible: Int = 5
  private var lowDataTrigger: () => Unit = () => {}
  private var onClick: (A, View) => Unit = (A, View) => {}
  private var swipeUpdate: (SwipeDirectionField, Float, View) => Unit = (Direction, Percentage, View) => {}
  private var mAnimateDirection: AnimateDirectionField = NoAnimation

  private final val mDataSetObserver: DataSetObserver = new DataSetObserver {
    override def onChanged() {
      super.onChanged()
      clearStack()
      ensureFull()
    }

    override def onInvalidated() {
      super.onInvalidated()
      clearStack()
    }
  }

  // constuctor section
  if (attrs != null) {
    initFromXml(attrs)
  } else {
    setOrientation(Disordered)
    setGravity(Gravity.CENTER)
  }
  init()
  // constructor end
  def getChildren = for {
    i <- 0 to getChildCount-1 if getChildCount > 0
  } yield getChildAt(i)

  case class IndexedChild(index: Int, child: View)
  def getChildrenWithIndex = for {
    i <- 0 to getChildCount-1 if getChildCount > 0
  } yield IndexedChild(i, getChildAt(i))

  private def viewAnimate(direction: AnimateDirectionField) = {
    val targetX = direction match {
      case AnimateLeft => getScreenWidth* -1
      case AnimateRight => getScreenWidth
      case NoAnimation => 0
    }
    CardDeckView.mAnimating = true
    CardDeckView.TopCard(this) match {
      case Some(v) =>
        v.animate()
         .setDuration(200)
         .alpha(.75f)
         .setInterpolator(new LinearInterpolator())
         .x(targetX)
         .rotation(Math.copySign(45, targetX))
         .setListener(CardDeckView.AnimationListener(this,v))
      case None =>
    }
  }

  private def getScreenWidth: Int = {
    getContext.getResources.getDisplayMetrics.widthPixels
  }

  def allowFling: Boolean = {
    !CardDeckView.mAnimating && getChildCount > 0
  }

  def doDislike(): Option[CardModel[A]] = {
    if (allowFling) {
      CardDeckView.mAnimating = true
      CardDeckView.TopCard(this) match {
        case None => None
        case Some(v) =>
          val card = getAdapter.pop
          card.dislike(card.data, v)
          v.postDelayed(new Runnable {
            override def run(): Unit = viewAnimate(AnimateLeft)
          }, 150)
          Some(card)
      }
    } else None
  }

  def doLike(): Option[CardModel[A]] = {
    if (allowFling) {
      CardDeckView.mAnimating = true
      CardDeckView.TopCard(this) match {
        case None => None
        case Some(v) =>
          val card = getAdapter.pop
          card.like(card.data, v)
          v.postDelayed(new Runnable {
            override def run(): Unit = viewAnimate(AnimateRight)
          }, 150)
          Some(card)
      }
    } else None
  }

  def setOrientation(orientation: Field) {
    if (mOrientation ne orientation) {
      mOrientation = orientation
      orientation match {
        case Disordered =>
          getChildrenWithIndex.foreach(ic => {
            ic.child.setRotation(getDisorderedRotation(getAdapter.getItem(ic.index).hashCode()))
          })
        case Ordered => getChildren.foreach(_.setRotation(0))
        case _ =>
      }
      requestLayout()
    }
  }

  def setGravity(gravity: Int) { mGravity = gravity}

  private def getDisorderedRotation(seed: Int): Float = {
    Math.toDegrees(new Random(seed).nextGaussian * ROTATION_RADIANS).asInstanceOf[Float]
  }

  protected override def onLayout(changed: Boolean, l: Int, t: Int, r: Int, b: Int): Unit = {
    super.onLayout(changed, l ,t ,r, b)
    getChildren.foreach(v => {
      boundsRect.set(0,0, getWidth, getHeight)
      val (w, h) = (v.getMeasuredWidth, getMeasuredHeight)
      Gravity.apply(mGravity, w, h, boundsRect, childRect)
      v.layout(childRect.left, childRect.top, childRect.right, childRect.bottom)
    })
  }

  private def getMeasureSpec(spec: (Int, Int)): (Int, Int) = {
    def calc(x: Int, y: Int): Int =
      ((x * Math.cos(ROTATION_RADIANS) - y * Math.sin(ROTATION_RADIANS))
        / Math.cos(2 * ROTATION_RADIANS)).toInt
    (calc(spec._1, spec._2), calc(spec._2, spec._1))
  }

  protected override def onMeasure(widthMeasureSpec: Int, heightMeasureSpec: Int): Unit = {
    super.onMeasure(widthMeasureSpec, heightMeasureSpec)
    val requestedWidth = getMeasuredWidth - getPaddingLeft - getPaddingRight
    val requestedHeight = getMeasuredHeight - getPaddingTop - getPaddingBottom
    val (childWidth, childHeight) = mOrientation match {
      case Disordered =>
        val r = if (requestedWidth >= requestedHeight) (requestedHeight, requestedWidth)
          else (requestedWidth, requestedHeight)
        getMeasureSpec(r)
      case Ordered =>
        (requestedWidth, requestedHeight)
    }
    val (childWidthMeasureSpec, childHeightMeasureSpec) =
      (MeasureSpec.makeMeasureSpec(childWidth, MeasureSpec.AT_MOST),
       MeasureSpec.makeMeasureSpec(childHeight, MeasureSpec.AT_MOST))
    getChildren.foreach(_.measure(childWidthMeasureSpec, childHeightMeasureSpec))
  }

  private def clearStack() {
    removeAllViewsInLayout()
  }

  private def ensureFull() {
    if (mAdapter.getCount < mLowDataLimit) lowDataTrigger()
    if (getChildCount < mMaxVisible && mAdapter.getCount >= mMaxVisible) {
      (for {i <- getChildCount to mMaxVisible} yield IndexedChild(i, mAdapter.getView(i, null, this)))
        .foreach(ic => {
        if (mOrientation == Orientation.Disordered) {
            ic.child.setRotation(getDisorderedRotation(getAdapter.getItem(ic.index).hashCode()))
        }
        addViewInLayout(ic.child, 0, new CardDeckView.LayoutParams(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT), false)
        requestLayout()
        ic.child.setLayerType(View.LAYER_TYPE_HARDWARE, null)
      })
    }
  }

  override def getAdapter: CardDeckAdapter[A] = mAdapter

  override def setAdapter(adapter: CardDeckAdapter[A]) {
    if (mAdapter != null) mAdapter.unregisterDataSetObserver(mDataSetObserver)
    clearStack()
    mAdapter = adapter
    adapter.registerDataSetObserver(mDataSetObserver)
    ensureFull()
    requestLayout()
  }

  override def setSelection(position: Int): Unit = {}

  override def getSelectedView: View = { CardDeckView.TopCard(this).get }

  private def init() {
    val viewConfiguration: ViewConfiguration = ViewConfiguration.get(getContext)
    CardDeckView.mFlingSlop = viewConfiguration.getScaledMinimumFlingVelocity*2
    CardDeckView.mTouchSlop = viewConfiguration.getScaledTouchSlop*2
    mGestureDetector = new GestureDetector(getContext, new CardDeckView.GestureListener(this))
  }

  private def initFromXml(attr: AttributeSet) {
    val a: TypedArray = getContext.obtainStyledAttributes(attr, R.styleable.CardDeckView)
    setGravity(a.getInteger(R.styleable.CardDeckView_android_gravity, Gravity.CENTER))
    val orientation: Int = a.getInteger(R.styleable.CardDeckView_orientation, 1)
    setOrientation(orientation match { case 0 => Ordered; case 1 => Disordered})
    mUseFling = a.getBoolean(R.styleable.CardDeckView_useFling, true)
    mMaxVisible = a.getInteger(R.styleable.CardDeckView_stackSize, 5)
    a.recycle()
  }

  def setLowDataTrigger(f: () => Unit) {
    lowDataTrigger = f
    if (getAdapter != null && getAdapter.getCount < mLowDataLimit) lowDataTrigger()
  }
  def getLowDataTrigger: () => Unit = lowDataTrigger

  var mLastTouchX = 0f
  var mLastTouchY = 0f
  final val INVALID_POINTER_ID = -1
  var mActivePointerId = INVALID_POINTER_ID
  var mDragging = false

  var startX: Float = 0
  var startY: Float = 0

  private def isClick(startX: Float, endX: Float, startY: Float, endY: Float): Boolean = {
    if (Math.abs(startX - endX) > 5 || Math.abs(startY - endY) > 5) false
    else true
  }

  def setOnClick(f: (A, View) => Unit) {onClick = f}
  def getOnClick: (A, View) => Unit = onClick

  def setOnSwipeUpdate(f: (SwipeDirectionField, Float, View) => Unit) {swipeUpdate = f}
  def getOnSwipeUpdate: (SwipeDirectionField, Float, View) => Unit = swipeUpdate

  override def onTouchEvent(event: MotionEvent): Boolean = {
    (CardDeckView.TopCard(this), CardDeckView.mAnimating) match {
      case (_, true) => false
      case (None, _) => false
      case (Some(v), _) =>
        if (mUseFling && mGestureDetector.onTouchEvent(event)) true
        else {
          var pointerIndex: Int = 0
          var (x, y, dx, dy) = (0f,0f,0f,0f)
          event.getActionMasked match {
            case MotionEvent.ACTION_DOWN =>
              v.getHitRect(childRect)
              pointerIndex = event.getActionIndex
              x = event.getX(pointerIndex)
              y = event.getY(pointerIndex)
              startX = x; startY = y
              if (!childRect.contains(x.toInt, y.toInt)) false
              else {
                mLastTouchX = x
                mLastTouchY = y
                mActivePointerId = event.getPointerId(pointerIndex)

                val points = Array(x - v.getLeft, y - v.getTop)
                v.getMatrix.invert(mMatrix)
                mMatrix.mapPoints(points)
                v.setPivotX(points(0))
                v.setPivotY(points(1))
              }
              true
            case MotionEvent.ACTION_MOVE =>
              pointerIndex = event.getActionIndex
              x = event.getX(pointerIndex)
              y = event.getY(pointerIndex)

              dx = x - mLastTouchX
              dy = y - mLastTouchY

              if (Math.abs(dx) > CardDeckView.mTouchSlop || Math.abs(dy) > CardDeckView.mTouchSlop) {
                mDragging = true
              }

              if(!mDragging) true
              else {
                v.setTranslationX(v.getTranslationX + dx)
                v.setTranslationY(v.getTranslationY + dy)
                v.setRotation(20 * v.getTranslationX / (getWidth / 2f))

                mLastTouchX = x
                mLastTouchY = y
                if (!mUseFling) {
                  def middle = v.getTranslationX + v.getLeft + v.getWidth/2
                  def initialMiddle = v.getLeft + v.getWidth/2
                  def leftThreshold = getWidth/4
                  def rightThreshold = (getWidth/4 * 3) - initialMiddle
                  def percentage = Math.min(
                    if (v.getTranslationX < 0) 1 - ((middle - leftThreshold) / leftThreshold)
                    else (middle - initialMiddle) / rightThreshold, 1)
                  def direction: SwipeDirectionField =
                    if (v.getTranslationX < 0) SwipeLeft
                    else SwipeRight
                  swipeUpdate(direction, percentage, v)
                  if (middle < (getWidth/4)) mAnimateDirection = AnimateLeft
                  else if (middle > (getWidth/4)*3) mAnimateDirection = AnimateRight
                  else mAnimateDirection = NoAnimation
                }
              }
              true
            case e if e == MotionEvent.ACTION_CANCEL || e == MotionEvent.ACTION_UP =>
              if (isClick(startX, event.getX(pointerIndex), startY, event.getY(pointerIndex))) {
                onClick(getAdapter.getItem(0).data, v)
                true
              }
              else if (!mDragging) true
              else {
                mDragging = false
                mActivePointerId = INVALID_POINTER_ID
                if (!mUseFling && mAnimateDirection != NoAnimation) {
                  val cardModel = getAdapter.pop
                  mAnimateDirection match {
                    case AnimateLeft => cardModel.dislike(cardModel.data, v)
                    case AnimateRight => cardModel.like(cardModel.data, v)
                  }
                  viewAnimate(mAnimateDirection)
                } else {
                  val animator = ObjectAnimator.ofPropertyValuesHolder(v,
                    PropertyValuesHolder.ofFloat("translationX", 0),
                    PropertyValuesHolder.ofFloat("translationY", 0),
                    PropertyValuesHolder.ofFloat("rotation", Math.toDegrees(Random.nextGaussian() * ROTATION_RADIANS).toFloat),
                    PropertyValuesHolder.ofFloat("pivotX", v.getWidth / 2f),
                    PropertyValuesHolder.ofFloat("pivotY", v.getHeight / 2f)
                  ).setDuration(250)
                  animator.setInterpolator(new AccelerateInterpolator())
                  animator.start()
                  // this is to clear the update
                  swipeUpdate(SwipeLeft, 0F, v)
                }
              }
              true
            case MotionEvent.ACTION_POINTER_UP =>
              pointerIndex = event.getActionIndex
              val pointerId = event.getPointerId(pointerIndex)
              if (pointerId == mActivePointerId) {
                val newPointerIndex = if (pointerIndex == 0) 1 else 0
                mLastTouchX = event.getX(newPointerIndex)
                mLastTouchY = event.getY(newPointerIndex)
                mActivePointerId = event.getPointerId(newPointerIndex)
              }
              true
            case _ =>
              Log.d("CardDeckView", "onTouchEvent event type not implemented")
              false
          }
        }
    }
  }

}
