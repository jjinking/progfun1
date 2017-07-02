package frp

// Signals are immutable

// Signal dependent on other signals
def inRectangle(LL: Position, UR: Position): Signal[Boolean] =
  Signal {
    val pos = mousePosition()
    LL <= pos && pos <= UR
  }

val sig = Signal(3) // constant signal

// Var are signals that can be changed via `update` function
varl sig = var(3)
sig.update(5) // same as `sig() = 5`
// whereas for dereferencing: `sig()`

class BankAccount {
  val balance = Var(0)
  def deposit(amount: Int): unit =
    if (amount > 0) {
      val b = balance()
      balance() = b + amount // Using Var class's `update` function
    }
  def withdraw(amount: Int): Unit =
    if (0 < amount && amount <= balance()) {
      val b = balance()
      balance() = b - amount
    } else throw new Error("insufficient funds")
}

// Worksheet
object accounts {
  def consolidated(accounts: List[BankAccount]): Signal[Int] =
    Signal(accts.map(_.balance()).sum)

  val a = new BankAccount()
  val b = new BankAccount()
  val c = consolidated(List(a, b))

  c() // returns the total balance on all accounts

  val xchange = Signal(246.00)
  val inDollar = Signal(c() * xchange())
  inDollar() // calling method `apply` in Signal class
  b withraw 10
  inDollar()
}

// -------------------------------------------------------------------------------- //
class Signal[T](expr: => T) {
  import Signal._
  // current values
  private var myExpr: () => T = _ // _ means uninitialized
  private var myValue: T = _
  private var observers: Set[Signal[_]] = Set()
  update(expr)

  protected def update(expr: => T): Unit = {
    myExpr = () => expr
    computeValue()
  }

  /**
   * Adds current object to caller stack, and runs `expr`
   *
   */
  protected def computeValue(): Unit = {
    val newValue = caller.withValue(this)(myExpr())  //  `caller` is global state in object Signal
    if (myValue != newValue) {
      myValue = newValue
      val obs = observers
      observers = Set()
      obs.foreach(_.computeValue()) // Evaluating the signals adds them back into observers, since it will call the apply method on the dependents
    }
  }

  /**
   * Returns current value of the signal
   * val a = Signal(...)
   * val b = Signal(...)
   * val c = Signal(a() + b())
   * c() // same as c.apply(), and c is the 'caller'
   *
   */
  def apply(): T = {
    observers += caller.value  // Add current calling signal to set of observers for current object
    assert(!caller.value.observers.contains(this), "cyclic signal definition")
    myValue
  }
}


object NoSignal extends Signal[Nothing](???) {
  override def computeValue() = ()
}


object Signal {
  // global state of callers
  private val caller = new StackableVariable[Signal[_]](NoSignal)
  // DynamicVariable[Signal[_]](NoSignal) for thread-safe

  def apply[T](expr: => T) = new Signal(expr)
}

// -------------------------------------------------------------------------------- //
// Var is pretty much same as signal, but exposes `update` method to clients

class Var[T](expr: => T) extends Signal[T](expr) {
  // Override update to to make it public, so that clients can update
  override def update(expr: => T): Unit = super.update(expr)
  def update(expr: => T): Unit = ???
}

object Var {
  def apply[T](expr: => T) = new Var(expr)
}

// -------------------------------------------------------------------------------- //

class StackableVariable[T](init: T) {

  private var values: List[T] = List(init)

  def value: T = values.head

  // Run a block `op` with `newValue` at head
  def withValue[R](newValue: T)(op: => R): R = {
    values = newValue :: values
    try op finally values = values.tail
  }
}
