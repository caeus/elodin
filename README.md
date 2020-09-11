# ELODIN

# Why named Elodin?

Because he's the master namer, in the Kingskiller chronicles. This project emerged as the side effect
of another one called Kvothe. So that's why

# Why Elodin?

There are two main purposes
1. Fun
2. Workflows

I used to work for a company called Kiwibot. They do deliveries, and a delivery has a very complex
life cycle. In this company, the life cycle was not clear... why? Because the code that described
it lived among many projects, and even in each project, it was all scattered around.

So I started thinking... how can I define workflows as code? And I honestly didn't check existing
solutions.

Anyway, what came to mind for me was (given I have a small grasp on continuations and monads), was
to define a monad like this (using scala notation)

````scala
trait JobOp[+V]
sealed trait Workflow[+A]
case class Job[+V,+A](op:JobOp[V],cont: V=> Workflow[A]) extends Workflow[A]
case class Result[+A](value:A) extends Workflow[A]

def job[T](op:JobOp[T]): Workflow[T] = Job[T,T](op,v=>Result(v))
```` 

I can `flatMap` this structure like this

````scala
sealed trait Workflow[+A]{
  def flatMap[B](f: A => Workflow[B]):Workflow[B]
}
case class Job[+V,+A](op:JobOp[V],cont: V=> Workflow[A]) extends Workflow[A] {
  override def flatMap[B](f:  A => Workflow[B]) = {
    Job[V,B](op,{v=>
      cont(v).flatMap(f)
    })
  }
}
case class Result[+T](value:T) extends Workflow[T] {
  override def flatMap[B](f:  T => Workflow[B])  = f(value)
}
````
Of course, error handling is missing in this structure, but extending it to have one
error channel is pretty easy. Anyway, in order to have a workflow be able to be described like thi
I'd need to be able to persist a `Workflow`. Most specifically, persist a `Job`.
And to achieve that, I need to be able to serialize `JobOb` (easy peasy) and the continuation...
which is a lambda. And that's it. It all comes down to serializing lambdas. Which I just couldn't
do in any language I knew. (And maybe I did a poor research)

Also, I've always wished there was something like Javascript (untyped), but pure.

