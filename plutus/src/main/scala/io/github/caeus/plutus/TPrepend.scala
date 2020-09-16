package io.github.caeus.plutus

sealed trait TPrepend[A, B] {
  type Out
  def apply(a: A, b: B): Out
}

object TPrepend extends TPrependLowPriority2 {}

sealed trait TPrependLowPriority2 extends TPrependLowPriority1 {
  implicit object nothingNothing extends TPrepend[Unit, Unit] {
    override type Out = Unit
    override def apply(a: Unit, b: Unit): Unit = ()
  }

  implicit def nothingPrepend[A]: Aux[Unit, A, A] =
    new TPrepend[Unit, A] {
      override type Out = A
      override def apply(a: Unit, b: A): A = b
    }
  implicit def appendNothing[A]: Aux[A, Unit, A] =
    new TPrepend[A, Unit] {
      override type Out = A

      override def apply(a: A, b: Unit): A = a
    }

}
sealed trait TPrependLowPriority1 extends TPrependLowPriority0 {
  //  println((for (n <- 3 to 20) yield {
  //    val params = (0 until n).map(i => s"T$i").mkString(", ")
  //    val iniTup = (0 until n - 1).map(i => s"T$i").mkString(", ")
  //    val aVals  = (1 until n).map(i => s"a._$i").mkString(", ")
  //    s"""implicit def tuple${n - 1}Prepend[$params]:Aux[($iniTup), T${n - 1}, ($params)] =
  //       |new TPrepend[($iniTup), T${n - 1}]{
  //       | override type Out = ($params)
  //       | override def append(a: ($iniTup), b: T${n - 1}): Out = ($aVals, b)
  //       |}
  //     """.stripMargin
  //  }).mkString("\n"))
  implicit def tuple2Prepend[T0, T1, T2]:Aux[T0, (T1, T2), (T0, T1, T2)] =
    new TPrepend[T0, (T1, T2)]{
      override type Out = (T0, T1, T2)
      override def apply(a: T0, b: (T1, T2)): Out = (a, b._1, b._2)
    }

  implicit def tuple3Prepend[T0, T1, T2, T3]:Aux[T0, (T1, T2, T3), (T0, T1, T2, T3)] =
    new TPrepend[T0, (T1, T2, T3)]{
      override type Out = (T0, T1, T2, T3)
      override def apply(a: T0, b: (T1, T2, T3)): Out = (a, b._1, b._2, b._3)
    }

  implicit def tuple4Prepend[T0, T1, T2, T3, T4]:Aux[T0, (T1, T2, T3, T4), (T0, T1, T2, T3, T4)] =
    new TPrepend[T0, (T1, T2, T3, T4)]{
      override type Out = (T0, T1, T2, T3, T4)
      override def apply(a: T0, b: (T1, T2, T3, T4)): Out = (a, b._1, b._2, b._3, b._4)
    }

  implicit def tuple5Prepend[T0, T1, T2, T3, T4, T5]:Aux[T0, (T1, T2, T3, T4, T5), (T0, T1, T2, T3, T4, T5)] =
    new TPrepend[T0, (T1, T2, T3, T4, T5)]{
      override type Out = (T0, T1, T2, T3, T4, T5)
      override def apply(a: T0, b: (T1, T2, T3, T4, T5)): Out = (a, b._1, b._2, b._3, b._4, b._5)
    }

  implicit def tuple6Prepend[T0, T1, T2, T3, T4, T5, T6]:Aux[T0, (T1, T2, T3, T4, T5, T6), (T0, T1, T2, T3, T4, T5, T6)] =
    new TPrepend[T0, (T1, T2, T3, T4, T5, T6)]{
      override type Out = (T0, T1, T2, T3, T4, T5, T6)
      override def apply(a: T0, b: (T1, T2, T3, T4, T5, T6)): Out = (a, b._1, b._2, b._3, b._4, b._5, b._6)
    }

  implicit def tuple7Prepend[T0, T1, T2, T3, T4, T5, T6, T7]:Aux[T0, (T1, T2, T3, T4, T5, T6, T7), (T0, T1, T2, T3, T4, T5, T6, T7)] =
    new TPrepend[T0, (T1, T2, T3, T4, T5, T6, T7)]{
      override type Out = (T0, T1, T2, T3, T4, T5, T6, T7)
      override def apply(a: T0, b: (T1, T2, T3, T4, T5, T6, T7)): Out = (a, b._1, b._2, b._3, b._4, b._5, b._6, b._7)
    }

  implicit def tuple8Prepend[T0, T1, T2, T3, T4, T5, T6, T7, T8]:Aux[T0, (T1, T2, T3, T4, T5, T6, T7, T8), (T0, T1, T2, T3, T4, T5, T6, T7, T8)] =
    new TPrepend[T0, (T1, T2, T3, T4, T5, T6, T7, T8)]{
      override type Out = (T0, T1, T2, T3, T4, T5, T6, T7, T8)
      override def apply(a: T0, b: (T1, T2, T3, T4, T5, T6, T7, T8)): Out = (a, b._1, b._2, b._3, b._4, b._5, b._6, b._7, b._8)
    }

  implicit def tuple9Prepend[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9]:Aux[T0, (T1, T2, T3, T4, T5, T6, T7, T8, T9), (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9)] =
    new TPrepend[T0, (T1, T2, T3, T4, T5, T6, T7, T8, T9)]{
      override type Out = (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9)
      override def apply(a: T0, b: (T1, T2, T3, T4, T5, T6, T7, T8, T9)): Out = (a, b._1, b._2, b._3, b._4, b._5, b._6, b._7, b._8, b._9)
    }

  implicit def tuple10Prepend[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]:Aux[T0, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10), (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] =
    new TPrepend[T0, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)]{
      override type Out = (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)
      override def apply(a: T0, b: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)): Out = (a, b._1, b._2, b._3, b._4, b._5, b._6, b._7, b._8, b._9, b._10)
    }

  implicit def tuple11Prepend[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]:Aux[T0, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11), (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] =
    new TPrepend[T0, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)]{
      override type Out = (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)
      override def apply(a: T0, b: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)): Out = (a, b._1, b._2, b._3, b._4, b._5, b._6, b._7, b._8, b._9, b._10, b._11)
    }

  implicit def tuple12Prepend[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]:Aux[T0, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12), (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] =
    new TPrepend[T0, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)]{
      override type Out = (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)
      override def apply(a: T0, b: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)): Out = (a, b._1, b._2, b._3, b._4, b._5, b._6, b._7, b._8, b._9, b._10, b._11, b._12)
    }

  implicit def tuple13Prepend[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]:Aux[T0, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13), (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] =
    new TPrepend[T0, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)]{
      override type Out = (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)
      override def apply(a: T0, b: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)): Out = (a, b._1, b._2, b._3, b._4, b._5, b._6, b._7, b._8, b._9, b._10, b._11, b._12, b._13)
    }

  implicit def tuple14Prepend[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]:Aux[T0, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14), (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] =
    new TPrepend[T0, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)]{
      override type Out = (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)
      override def apply(a: T0, b: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)): Out = (a, b._1, b._2, b._3, b._4, b._5, b._6, b._7, b._8, b._9, b._10, b._11, b._12, b._13, b._14)
    }

  implicit def tuple15Prepend[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]:Aux[T0, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15), (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] =
    new TPrepend[T0, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)]{
      override type Out = (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)
      override def apply(a: T0, b: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)): Out = (a, b._1, b._2, b._3, b._4, b._5, b._6, b._7, b._8, b._9, b._10, b._11, b._12, b._13, b._14, b._15)
    }

  implicit def tuple16Prepend[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]:Aux[T0, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16), (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] =
    new TPrepend[T0, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)]{
      override type Out = (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)
      override def apply(a: T0, b: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)): Out = (a, b._1, b._2, b._3, b._4, b._5, b._6, b._7, b._8, b._9, b._10, b._11, b._12, b._13, b._14, b._15, b._16)
    }

  implicit def tuple17Prepend[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]:Aux[T0, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17), (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] =
    new TPrepend[T0, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)]{
      override type Out = (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)
      override def apply(a: T0, b: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)): Out = (a, b._1, b._2, b._3, b._4, b._5, b._6, b._7, b._8, b._9, b._10, b._11, b._12, b._13, b._14, b._15, b._16, b._17)
    }

  implicit def tuple18Prepend[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]:Aux[T0, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18), (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] =
    new TPrepend[T0, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)]{
      override type Out = (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)
      override def apply(a: T0, b: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)): Out = (a, b._1, b._2, b._3, b._4, b._5, b._6, b._7, b._8, b._9, b._10, b._11, b._12, b._13, b._14, b._15, b._16, b._17, b._18)
    }

  implicit def tuple19Prepend[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]:Aux[T0, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19), (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] =
    new TPrepend[T0, (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)]{
      override type Out = (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)
      override def apply(a: T0, b: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)): Out = (a, b._1, b._2, b._3, b._4, b._5, b._6, b._7, b._8, b._9, b._10, b._11, b._12, b._13, b._14, b._15, b._16, b._17, b._18, b._19)
    }

}

sealed trait TPrependLowPriority0 {
  type Aux[A, B, O] = TPrepend[A, B] { type Out = O }

  implicit def appendAny[A, B]: Aux[A, B, (A, B)] =
    new TPrepend[A, B] {
      override type Out = (A, B)

      override def apply(a: A, b: B): Out = (a, b)
    }
}
