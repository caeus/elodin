package com.github.caeus.plutus

sealed trait TConcat[A, B] {
  type Out
  def apply(a: A, b: B): Out
}

object TConcat extends TConcatLowPriority2 {}

sealed trait TConcatLowPriority2 extends TConcatLowPriority1 {
  implicit object nothingNothing extends TConcat[Unit, Unit] {
    override type Out = Unit
    override def apply(a: Unit, b: Unit): Unit = ()
  }

  implicit def nothingAppend[A]: Aux[Unit, A, A] = new TConcat[Unit, A] {
    override type Out = A
    override def apply(a: Unit, b: A): A = b
  }
  implicit def appendNothing[A]: Aux[A, Unit, A] = new TConcat[A, Unit] {
    override type Out = A

    override def apply(a: A, b: Unit): A = a
  }

}
sealed trait TConcatLowPriority1 extends TConcatLowPriority0 {
  //  println((for (n <- 3 to 20) yield {
  //    val params = (0 until n).map(i => s"T$i").mkString(", ")
  //    val iniTup = (0 until n - 1).map(i => s"T$i").mkString(", ")
  //    val aVals  = (1 until n).map(i => s"a._$i").mkString(", ")
  //    s"""implicit def tuple${n - 1}Append[$params]:Aux[($iniTup), T${n - 1}, ($params)] =
  //       |new TAppend[($iniTup), T${n - 1}]{
  //       | override type Out = ($params)
  //       | override def append(a: ($iniTup), b: T${n - 1}): Out = ($aVals, b)
  //       |}
  //     """.stripMargin
  //  }).mkString("\n"))
  implicit def tuple2Append[T0, T1, T2]: Aux[(T0, T1), T2, (T0, T1, T2)] =
    new TConcat[(T0, T1), T2] {
      override type Out = (T0, T1, T2)
      override def apply(a: (T0, T1), b: T2): Out = (a._1, a._2, b)
    }
  implicit def tuple3Append[T0, T1, T2, T3]: Aux[(T0, T1, T2), T3, (T0, T1, T2, T3)] =
    new TConcat[(T0, T1, T2), T3] {
      override type Out = (T0, T1, T2, T3)
      override def apply(a: (T0, T1, T2), b: T3): Out = (a._1, a._2, a._3, b)
    }

  implicit def tuple4Append[T0, T1, T2, T3, T4]: Aux[(T0, T1, T2, T3), T4, (T0, T1, T2, T3, T4)] =
    new TConcat[(T0, T1, T2, T3), T4] {
      override type Out = (T0, T1, T2, T3, T4)
      override def apply(a: (T0, T1, T2, T3), b: T4): Out = (a._1, a._2, a._3, a._4, b)
    }

  implicit def tuple5Append[T0, T1, T2, T3, T4, T5]
    : Aux[(T0, T1, T2, T3, T4), T5, (T0, T1, T2, T3, T4, T5)] =
    new TConcat[(T0, T1, T2, T3, T4), T5] {
      override type Out = (T0, T1, T2, T3, T4, T5)
      override def apply(a: (T0, T1, T2, T3, T4), b: T5): Out = (a._1, a._2, a._3, a._4, a._5, b)
    }

  implicit def tuple6Append[T0, T1, T2, T3, T4, T5, T6]
    : Aux[(T0, T1, T2, T3, T4, T5), T6, (T0, T1, T2, T3, T4, T5, T6)] =
    new TConcat[(T0, T1, T2, T3, T4, T5), T6] {
      override type Out = (T0, T1, T2, T3, T4, T5, T6)
      override def apply(a: (T0, T1, T2, T3, T4, T5), b: T6): Out =
        (a._1, a._2, a._3, a._4, a._5, a._6, b)
    }

  implicit def tuple7Append[T0, T1, T2, T3, T4, T5, T6, T7]
    : Aux[(T0, T1, T2, T3, T4, T5, T6), T7, (T0, T1, T2, T3, T4, T5, T6, T7)] =
    new TConcat[(T0, T1, T2, T3, T4, T5, T6), T7] {
      override type Out = (T0, T1, T2, T3, T4, T5, T6, T7)
      override def apply(a: (T0, T1, T2, T3, T4, T5, T6), b: T7): Out =
        (a._1, a._2, a._3, a._4, a._5, a._6, a._7, b)
    }

  implicit def tuple8Append[T0, T1, T2, T3, T4, T5, T6, T7, T8]
    : Aux[(T0, T1, T2, T3, T4, T5, T6, T7), T8, (T0, T1, T2, T3, T4, T5, T6, T7, T8)] =
    new TConcat[(T0, T1, T2, T3, T4, T5, T6, T7), T8] {
      override type Out = (T0, T1, T2, T3, T4, T5, T6, T7, T8)
      override def apply(a: (T0, T1, T2, T3, T4, T5, T6, T7), b: T8): Out =
        (a._1, a._2, a._3, a._4, a._5, a._6, a._7, a._8, b)
    }

  implicit def tuple9Append[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9]
    : Aux[(T0, T1, T2, T3, T4, T5, T6, T7, T8), T9, (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9)] =
    new TConcat[(T0, T1, T2, T3, T4, T5, T6, T7, T8), T9] {
      override type Out = (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9)
      override def apply(a: (T0, T1, T2, T3, T4, T5, T6, T7, T8), b: T9): Out =
        (a._1, a._2, a._3, a._4, a._5, a._6, a._7, a._8, a._9, b)
    }

  implicit def tuple10Append[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]
    : Aux[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9),
          T10,
          (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] =
    new TConcat[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9), T10] {
      override type Out = (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)
      override def apply(a: (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9), b: T10): Out =
        (a._1, a._2, a._3, a._4, a._5, a._6, a._7, a._8, a._9, a._10, b)
    }

  implicit def tuple11Append[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]
    : Aux[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10),
          T11,
          (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] =
    new TConcat[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10), T11] {
      override type Out = (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)
      override def apply(a: (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10), b: T11): Out =
        (a._1, a._2, a._3, a._4, a._5, a._6, a._7, a._8, a._9, a._10, a._11, b)
    }

  implicit def tuple12Append[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]
    : Aux[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11),
          T12,
          (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] =
    new TConcat[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11), T12] {
      override type Out = (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)
      override def apply(a: (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11), b: T12): Out =
        (a._1, a._2, a._3, a._4, a._5, a._6, a._7, a._8, a._9, a._10, a._11, a._12, b)
    }

  implicit def tuple13Append[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]
    : Aux[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12),
          T13,
          (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] =
    new TConcat[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12), T13] {
      override type Out = (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)
      override def apply(a: (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12), b: T13): Out =
        (a._1, a._2, a._3, a._4, a._5, a._6, a._7, a._8, a._9, a._10, a._11, a._12, a._13, b)
    }

  implicit def tuple14Append[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]
    : Aux[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13),
          T14,
          (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] =
    new TConcat[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13), T14] {
      override type Out = (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)
      override def apply(a: (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13),
                         b: T14): Out =
        (a._1, a._2, a._3, a._4, a._5, a._6, a._7, a._8, a._9, a._10, a._11, a._12, a._13, a._14, b)
    }

  implicit def tuple15Append[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]
    : Aux[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14),
          T15,
          (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] =
    new TConcat[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14), T15] {
      override type Out = (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)
      override def apply(a: (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14),
                         b: T15): Out =
        (a._1,
         a._2,
         a._3,
         a._4,
         a._5,
         a._6,
         a._7,
         a._8,
         a._9,
         a._10,
         a._11,
         a._12,
         a._13,
         a._14,
         a._15,
         b)
    }

  implicit def tuple16Append[T0,
                             T1,
                             T2,
                             T3,
                             T4,
                             T5,
                             T6,
                             T7,
                             T8,
                             T9,
                             T10,
                             T11,
                             T12,
                             T13,
                             T14,
                             T15,
                             T16]
    : Aux[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15),
          T16,
          (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] =
    new TConcat[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15), T16] {
      override type Out =
        (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)
      override def apply(a: (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15),
                         b: T16): Out =
        (a._1,
         a._2,
         a._3,
         a._4,
         a._5,
         a._6,
         a._7,
         a._8,
         a._9,
         a._10,
         a._11,
         a._12,
         a._13,
         a._14,
         a._15,
         a._16,
         b)
    }

  implicit def tuple17Append[T0,
                             T1,
                             T2,
                             T3,
                             T4,
                             T5,
                             T6,
                             T7,
                             T8,
                             T9,
                             T10,
                             T11,
                             T12,
                             T13,
                             T14,
                             T15,
                             T16,
                             T17]
    : Aux[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16),
          T17,
          (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] =
    new TConcat[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16), T17] {
      override type Out =
        (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)
      override def apply(
          a: (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16),
          b: T17): Out =
        (a._1,
         a._2,
         a._3,
         a._4,
         a._5,
         a._6,
         a._7,
         a._8,
         a._9,
         a._10,
         a._11,
         a._12,
         a._13,
         a._14,
         a._15,
         a._16,
         a._17,
         b)
    }

  implicit def tuple18Append[T0,
                             T1,
                             T2,
                             T3,
                             T4,
                             T5,
                             T6,
                             T7,
                             T8,
                             T9,
                             T10,
                             T11,
                             T12,
                             T13,
                             T14,
                             T15,
                             T16,
                             T17,
                             T18]
    : Aux[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17),
          T18,
          (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] =
    new TConcat[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17),
                T18] {
      override type Out =
        (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)
      override def apply(
          a: (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17),
          b: T18): Out =
        (a._1,
         a._2,
         a._3,
         a._4,
         a._5,
         a._6,
         a._7,
         a._8,
         a._9,
         a._10,
         a._11,
         a._12,
         a._13,
         a._14,
         a._15,
         a._16,
         a._17,
         a._18,
         b)
    }

  implicit def tuple19Append[T0,
                             T1,
                             T2,
                             T3,
                             T4,
                             T5,
                             T6,
                             T7,
                             T8,
                             T9,
                             T10,
                             T11,
                             T12,
                             T13,
                             T14,
                             T15,
                             T16,
                             T17,
                             T18,
                             T19]: Aux[
    (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18),
    T19,
    (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] =
    new TConcat[
      (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18),
      T19] {
      override type Out =
        (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)
      override def apply(
          a: (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18),
          b: T19): Out =
        (a._1,
         a._2,
         a._3,
         a._4,
         a._5,
         a._6,
         a._7,
         a._8,
         a._9,
         a._10,
         a._11,
         a._12,
         a._13,
         a._14,
         a._15,
         a._16,
         a._17,
         a._18,
         a._19,
         b)
    }
}

sealed trait TConcatLowPriority0 {
  type Aux[A, B, O] = TConcat[A, B] { type Out = O }

  implicit def appendAny[A, B]: Aux[A, B, (A, B)] = new TConcat[A, B] {
    override type Out = (A, B)

    override def apply(a: A, b: B): Out = (a, b)
  }
}
