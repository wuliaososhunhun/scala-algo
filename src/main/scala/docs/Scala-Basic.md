# Scala
- Scala stands for SCAlable LAnguage. Martin Odersky is the father of Scala.
- Hybrid Functional (Object-Oriented and Functional) Programming JVM Language. 
- has a Strong and Statically Type System. All types are checked at compile-time.
- Statically-Typed Language as Java vs Dynamic-Typed Language (Python, JavaScript)
- Pure Object-Oriented Programming Language (everything should be an Object). Java is not a Pure Object-Oriented Programming (OOP) Language because it 
supports the following two Non-OOP concepts:
  - Java supports primitive data types. They are not objects.
  - Java supports Static members. They are not related to objects.
- Java 8 does not support Pattern Matching, Function Currying, Implicits etc.
- REPL(Read-Evaluate-Print Loop) Interpreter

# Scala Advantages
- Simple and Concise Code e.g. syntax sugar
- 100% Type-Safe Language
- Supports all OOP Features and FP Features.
- Better Parallel and Concurrency Programming. e.g Future
- Highly Scalable and Maintainable code
- Distributed Applications
- Full Java Interoperability
- Powerful Scala DSLs available

# Scala Cons
- Less Backward Compatibility
- learning curve

# Type Inference
Types can be inferred by the Scala Compiler at compile-time. Types means Data type or Result type. We use Types at many places in Scala programs like Variable types, Object types, Method/Function Parameter types, Method/Function return types etc.

# Nothing and Nil
- Nothing is a Type (final class). It is defined at the bottom of the Scala Type System that means it is a subtype of anything in Scala. There are no 
instances of Nothing.
- Nil is a list contains Nothing

# Null
Null is a Type (final class) in Scala. Null type is available in “scala” package as “scala.Null”. It has one and only one instance that is null.

# Unit
Unit is used to represent “No value” or “No Useful value”. Unit is a final class defined in “scala” package that is “scala.Unit”.

Unit is something similar to Java’s void. But they have few differences.

- Java’s void does not any value. It is nothing. It is a keyword not a value.
- Scala’s Unit is a final class has one and only value ().  

# App
App is a trait defined in scala package like “scala.App”. It defines main method. If an Object or a Class extends this trait, then they will become as Scala Executable programs automatically because they will inherit main method from Application.

The main advantage of using App is that we don’t need to write main method. The main drawback of using App is that we should use same name “args” to refer command line argument because scala.App’s main() method uses this name.

# Express vs Statement
Expression is a value that means it will evaluate to a Value. As an Expression returns a value, We can assign it to a variable.

Statement defines one or more actions or operations. That means Statement performs actions. As it does not return a value, we cannot assign it to a Variable.

# PreDef
PreDef is an object defined in scala package as “scala.PreDef”. It is an utility object.

It defines many utility methods as shown below:

- Console IO (print,println etc)
- Collection utility methods
- String utility methods
- Implicit conversion methods
- Assertion utility methods etc.