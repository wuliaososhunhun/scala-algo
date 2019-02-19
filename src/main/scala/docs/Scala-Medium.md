# Primitive Constructor
Primary Constructor, a constructor which is defined with class definition itself. Each class must have one Primary Constructor: Either Parameter constructor 
or Parameterless constructor.

`class Person (firstName: String, lastName: String)`

Auxiliary (Secondary) Constructor, is declared with ‘def’ and ‘this’ keywords as shown below:

```
class Person (firstName: String, middleName:String, lastName: String){
  def this(firstName: String, lastName: String){
      this(firstName, "", lastName)
  }
}
```

All Auxiliary Constructors call their Primary Constructor either directly or indirectly through other Auxiliary Constructors.

# Diamond Problem - cake pattern

```
trait A{   
  def display(){ println("From A.display")  }
}
trait B extends A{ 
  override def display() { println("From B.display") }
}
trait C extends A{ 
  override def display() { println("From C.display") }
}
class D extends B with C{ }

object ScalaDiamonProblemTest extends App {
    val d = new D
    d display
}
```
 output is “From C.display” form trait C. Scala Compiler reads “extends B with C” from right to left and takes “display” method definition from lest most trait that is C.
 
# No static
The main reason to remove static is to make Scala as a Pure Object-Oriented Language. “static” keyword means that we can access that class members without 
creating an object or without using an object. This is completely against with OOP principles.

# apply vs unapply
apply method: To compose or assemble an object from it’s components. e.g. secondary constructor
unapply method: To decompose or dis-assemble an object into it’s components. e.g. pattern match

# Companion object
If a Scala class and object shares the same name and defined in the same source file, then that class is known as “Companion Class” and that object is known 
as “Companion Object”.

Companion object can access private members of it’s Companion class and Companion class can access it’s Companion object’s private members.

# function vs procedure
- A function is a computation unit without side-effect 
- a Procedure is also a computation unit with side-effects

# Pattern Matching
follows Visitor Design Pattern