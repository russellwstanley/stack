import org.scalatest._

class StackEmptyException extends Exception{

}


class Stack{
  def isEmpty = true
  def push(elem : Int) : Stack = new NonEmptyStack(elem, this)
  def pop : (Stack,Int) = throw new StackEmptyException()
}

class NonEmptyStack(elem : Int, inner : Stack) extends Stack{
  override def isEmpty = false
  override def pop : (Stack,Int) = (inner,elem)
}

class ExampleSpec extends FlatSpec with Matchers {

  "a stack" should "be empty upon creation" in {
    val stack = new Stack()
    stack.isEmpty should be (true)
  }
  it should "not be empty after adding a thing" in {
    val stack = new Stack()
    stack.push(1)
    stack.isEmpty should be (true)
  }
  it should "throw an exception when you pop an empty stack" in {
    val empty = new Stack()
    intercept[StackEmptyException]{
      empty.pop
    }
  }
  it should "get the same value back when you push then pop" in {
    val stack = new Stack()
    stack.push(1).pop._2 should be (1)
  }
  it should "get the same value back when you push then pop with non 1" in {
    val stack = new Stack()
    stack.push(2).pop._2 should be (2)
  }
  it should "be able to contain multiple elemets" in {
    val stack = new Stack()
    stack.push(2).push(3).pop._2 should be (3)
  }
  it should "support multiple pops" in {
    val empty = new Stack()
    val oneElem = empty.push(1)
    val twoElem = oneElem.push(2)
    val(newStack,res) = twoElem.pop
    res should be (2)
    val(newEmpty,newRes) = newStack.pop
    newRes should be(1)
    newEmpty.isEmpty should be (true)
  }

}