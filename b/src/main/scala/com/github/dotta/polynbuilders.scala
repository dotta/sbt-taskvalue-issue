import shapeless._

/**
  * Provides elegant syntax for creating polys from functions
  *
  * @author Aristotelis Dossas
  */
object PolyNBuilders {

 trait Poly1Builder[HL <: HList] extends PolyApply { self =>
   type λ = build.type
   val functions: HL
   class AtAux[A] {
     def apply[Out](λ: (A) => Out): Poly1Builder[((A) => Out) :: HL] = {
       new Poly1Builder[((A) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }

   def at[A] = new AtAux[A]

   object build extends Poly1 {
     val functions = self.functions
     implicit def allCases[A, Out](implicit tL: Function1TypeAt[A, Out, HL]): Case.Aux[A, Out] =
       at(tL(functions))
   }
 }

 /* For internal use of Poly1Builder */
 trait Function1TypeAt[A, Out, HL <: HList] {
   def apply(l: HL): (A) => Out
 }

 object Function1TypeAt {
   implicit def at0[A, Out, Tail <: HList]: Function1TypeAt[A, Out, ((A) => Out)::Tail] =
     new Function1TypeAt[A, Out, ((A) => Out)::Tail] {
       def apply(l: ((A) => Out)::Tail): (A) => Out = l.head
     }

   implicit def atOther[A, Out, Tail <: HList, Head](implicit tprev: Function1TypeAt[A, Out, Tail]): Function1TypeAt[A, Out, Head::Tail] =
     new Function1TypeAt[A, Out, Head::Tail] {
       def apply(l: Head::Tail): (A) => Out = tprev(l.tail)
     }
 }

 trait Poly2Builder[HL <: HList] extends PolyApply { self =>
   type λ = build.type
   val functions: HL
   class AtAux[A, B] {
     def apply[Out](λ: (A, B) => Out): Poly2Builder[((A, B) => Out) :: HL] = {
       new Poly2Builder[((A, B) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }

   def at[A, B] = new AtAux[A, B]

   object build extends Poly2 {
     val functions = self.functions
     implicit def allCases[A, B, Out](implicit tL: Function2TypeAt[A, B, Out, HL]): Case.Aux[A, B, Out] =
       at(tL(functions))
   }
 }

 /* For internal use of Poly2Builder */
 trait Function2TypeAt[A, B, Out, HL <: HList] {
   def apply(l: HL): (A, B) => Out
 }

 object Function2TypeAt {
   implicit def at0[A, B, Out, Tail <: HList]: Function2TypeAt[A, B, Out, ((A, B) => Out)::Tail] =
     new Function2TypeAt[A, B, Out, ((A, B) => Out)::Tail] {
       def apply(l: ((A, B) => Out)::Tail): (A, B) => Out = l.head
     }

   implicit def atOther[A, B, Out, Tail <: HList, Head](implicit tprev: Function2TypeAt[A, B, Out, Tail]): Function2TypeAt[A, B, Out, Head::Tail] =
     new Function2TypeAt[A, B, Out, Head::Tail] {
       def apply(l: Head::Tail): (A, B) => Out = tprev(l.tail)
     }
 }

 trait Poly3Builder[HL <: HList] extends PolyApply { self =>
   type λ = build.type
   val functions: HL
   class AtAux[A, B, C] {
     def apply[Out](λ: (A, B, C) => Out): Poly3Builder[((A, B, C) => Out) :: HL] = {
       new Poly3Builder[((A, B, C) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }

   def at[A, B, C] = new AtAux[A, B, C]

   object build extends Poly3 {
     val functions = self.functions
     implicit def allCases[A, B, C, Out](implicit tL: Function3TypeAt[A, B, C, Out, HL]): Case.Aux[A, B, C, Out] =
       at(tL(functions))
   }
 }

 /* For internal use of Poly3Builder */
 trait Function3TypeAt[A, B, C, Out, HL <: HList] {
   def apply(l: HL): (A, B, C) => Out
 }

 object Function3TypeAt {
   implicit def at0[A, B, C, Out, Tail <: HList]: Function3TypeAt[A, B, C, Out, ((A, B, C) => Out)::Tail] =
     new Function3TypeAt[A, B, C, Out, ((A, B, C) => Out)::Tail] {
       def apply(l: ((A, B, C) => Out)::Tail): (A, B, C) => Out = l.head
     }

   implicit def atOther[A, B, C, Out, Tail <: HList, Head](implicit tprev: Function3TypeAt[A, B, C, Out, Tail]): Function3TypeAt[A, B, C, Out, Head::Tail] =
     new Function3TypeAt[A, B, C, Out, Head::Tail] {
       def apply(l: Head::Tail): (A, B, C) => Out = tprev(l.tail)
     }
 }

 trait Poly4Builder[HL <: HList] extends PolyApply { self =>
   type λ = build.type
   val functions: HL
   class AtAux[A, B, C, D] {
     def apply[Out](λ: (A, B, C, D) => Out): Poly4Builder[((A, B, C, D) => Out) :: HL] = {
       new Poly4Builder[((A, B, C, D) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }

   def at[A, B, C, D] = new AtAux[A, B, C, D]

   object build extends Poly4 {
     val functions = self.functions
     implicit def allCases[A, B, C, D, Out](implicit tL: Function4TypeAt[A, B, C, D, Out, HL]): Case.Aux[A, B, C, D, Out] =
       at(tL(functions))
   }
 }

 /* For internal use of Poly4Builder */
 trait Function4TypeAt[A, B, C, D, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D) => Out
 }

 object Function4TypeAt {
   implicit def at0[A, B, C, D, Out, Tail <: HList]: Function4TypeAt[A, B, C, D, Out, ((A, B, C, D) => Out)::Tail] =
     new Function4TypeAt[A, B, C, D, Out, ((A, B, C, D) => Out)::Tail] {
       def apply(l: ((A, B, C, D) => Out)::Tail): (A, B, C, D) => Out = l.head
     }

   implicit def atOther[A, B, C, D, Out, Tail <: HList, Head](implicit tprev: Function4TypeAt[A, B, C, D, Out, Tail]): Function4TypeAt[A, B, C, D, Out, Head::Tail] =
     new Function4TypeAt[A, B, C, D, Out, Head::Tail] {
       def apply(l: Head::Tail): (A, B, C, D) => Out = tprev(l.tail)
     }
 }

 trait Poly5Builder[HL <: HList] extends PolyApply { self =>
   type λ = build.type
   val functions: HL
   class AtAux[A, B, C, D, E] {
     def apply[Out](λ: (A, B, C, D, E) => Out): Poly5Builder[((A, B, C, D, E) => Out) :: HL] = {
       new Poly5Builder[((A, B, C, D, E) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }

   def at[A, B, C, D, E] = new AtAux[A, B, C, D, E]

   object build extends Poly5 {
     val functions = self.functions
     implicit def allCases[A, B, C, D, E, Out](implicit tL: Function5TypeAt[A, B, C, D, E, Out, HL]): Case.Aux[A, B, C, D, E, Out] =
       at(tL(functions))
   }
 }

 /* For internal use of Poly5Builder */
 trait Function5TypeAt[A, B, C, D, E, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E) => Out
 }

 object Function5TypeAt {
   implicit def at0[A, B, C, D, E, Out, Tail <: HList]: Function5TypeAt[A, B, C, D, E, Out, ((A, B, C, D, E) => Out)::Tail] =
     new Function5TypeAt[A, B, C, D, E, Out, ((A, B, C, D, E) => Out)::Tail] {
       def apply(l: ((A, B, C, D, E) => Out)::Tail): (A, B, C, D, E) => Out = l.head
     }

   implicit def atOther[A, B, C, D, E, Out, Tail <: HList, Head](implicit tprev: Function5TypeAt[A, B, C, D, E, Out, Tail]): Function5TypeAt[A, B, C, D, E, Out, Head::Tail] =
     new Function5TypeAt[A, B, C, D, E, Out, Head::Tail] {
       def apply(l: Head::Tail): (A, B, C, D, E) => Out = tprev(l.tail)
     }
 }

 trait Poly6Builder[HL <: HList] extends PolyApply { self =>
   type λ = build.type
   val functions: HL
   class AtAux[A, B, C, D, E, F] {
     def apply[Out](λ: (A, B, C, D, E, F) => Out): Poly6Builder[((A, B, C, D, E, F) => Out) :: HL] = {
       new Poly6Builder[((A, B, C, D, E, F) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }

   def at[A, B, C, D, E, F] = new AtAux[A, B, C, D, E, F]

   object build extends Poly6 {
     val functions = self.functions
     implicit def allCases[A, B, C, D, E, F, Out](implicit tL: Function6TypeAt[A, B, C, D, E, F, Out, HL]): Case.Aux[A, B, C, D, E, F, Out] =
       at(tL(functions))
   }
 }

 /* For internal use of Poly6Builder */
 trait Function6TypeAt[A, B, C, D, E, F, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F) => Out
 }

 object Function6TypeAt {
   implicit def at0[A, B, C, D, E, F, Out, Tail <: HList]: Function6TypeAt[A, B, C, D, E, F, Out, ((A, B, C, D, E, F) => Out)::Tail] =
     new Function6TypeAt[A, B, C, D, E, F, Out, ((A, B, C, D, E, F) => Out)::Tail] {
       def apply(l: ((A, B, C, D, E, F) => Out)::Tail): (A, B, C, D, E, F) => Out = l.head
     }

   implicit def atOther[A, B, C, D, E, F, Out, Tail <: HList, Head](implicit tprev: Function6TypeAt[A, B, C, D, E, F, Out, Tail]): Function6TypeAt[A, B, C, D, E, F, Out, Head::Tail] =
     new Function6TypeAt[A, B, C, D, E, F, Out, Head::Tail] {
       def apply(l: Head::Tail): (A, B, C, D, E, F) => Out = tprev(l.tail)
     }
 }

 trait Poly7Builder[HL <: HList] extends PolyApply { self =>
   type λ = build.type
   val functions: HL
   class AtAux[A, B, C, D, E, F, G] {
     def apply[Out](λ: (A, B, C, D, E, F, G) => Out): Poly7Builder[((A, B, C, D, E, F, G) => Out) :: HL] = {
       new Poly7Builder[((A, B, C, D, E, F, G) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }

   def at[A, B, C, D, E, F, G] = new AtAux[A, B, C, D, E, F, G]

   object build extends Poly7 {
     val functions = self.functions
     implicit def allCases[A, B, C, D, E, F, G, Out](implicit tL: Function7TypeAt[A, B, C, D, E, F, G, Out, HL]): Case.Aux[A, B, C, D, E, F, G, Out] =
       at(tL(functions))
   }
 }

 /* For internal use of Poly7Builder */
 trait Function7TypeAt[A, B, C, D, E, F, G, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G) => Out
 }

 object Function7TypeAt {
   implicit def at0[A, B, C, D, E, F, G, Out, Tail <: HList]: Function7TypeAt[A, B, C, D, E, F, G, Out, ((A, B, C, D, E, F, G) => Out)::Tail] =
     new Function7TypeAt[A, B, C, D, E, F, G, Out, ((A, B, C, D, E, F, G) => Out)::Tail] {
       def apply(l: ((A, B, C, D, E, F, G) => Out)::Tail): (A, B, C, D, E, F, G) => Out = l.head
     }

   implicit def atOther[A, B, C, D, E, F, G, Out, Tail <: HList, Head](implicit tprev: Function7TypeAt[A, B, C, D, E, F, G, Out, Tail]): Function7TypeAt[A, B, C, D, E, F, G, Out, Head::Tail] =
     new Function7TypeAt[A, B, C, D, E, F, G, Out, Head::Tail] {
       def apply(l: Head::Tail): (A, B, C, D, E, F, G) => Out = tprev(l.tail)
     }
 }

 trait Poly8Builder[HL <: HList] extends PolyApply { self =>
   type λ = build.type
   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H) => Out): Poly8Builder[((A, B, C, D, E, F, G, H) => Out) :: HL] = {
       new Poly8Builder[((A, B, C, D, E, F, G, H) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }

   def at[A, B, C, D, E, F, G, H] = new AtAux[A, B, C, D, E, F, G, H]

   object build extends Poly8 {
     val functions = self.functions
     implicit def allCases[A, B, C, D, E, F, G, H, Out](implicit tL: Function8TypeAt[A, B, C, D, E, F, G, H, Out, HL]): Case.Aux[A, B, C, D, E, F, G, H, Out] =
       at(tL(functions))
   }
 }

 /* For internal use of Poly8Builder */
 trait Function8TypeAt[A, B, C, D, E, F, G, H, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H) => Out
 }

 object Function8TypeAt {
   implicit def at0[A, B, C, D, E, F, G, H, Out, Tail <: HList]: Function8TypeAt[A, B, C, D, E, F, G, H, Out, ((A, B, C, D, E, F, G, H) => Out)::Tail] =
     new Function8TypeAt[A, B, C, D, E, F, G, H, Out, ((A, B, C, D, E, F, G, H) => Out)::Tail] {
       def apply(l: ((A, B, C, D, E, F, G, H) => Out)::Tail): (A, B, C, D, E, F, G, H) => Out = l.head
     }

   implicit def atOther[A, B, C, D, E, F, G, H, Out, Tail <: HList, Head](implicit tprev: Function8TypeAt[A, B, C, D, E, F, G, H, Out, Tail]): Function8TypeAt[A, B, C, D, E, F, G, H, Out, Head::Tail] =
     new Function8TypeAt[A, B, C, D, E, F, G, H, Out, Head::Tail] {
       def apply(l: Head::Tail): (A, B, C, D, E, F, G, H) => Out = tprev(l.tail)
     }
 }

 trait Poly9Builder[HL <: HList] extends PolyApply { self =>
   type λ = build.type
   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I) => Out): Poly9Builder[((A, B, C, D, E, F, G, H, I) => Out) :: HL] = {
       new Poly9Builder[((A, B, C, D, E, F, G, H, I) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }

   def at[A, B, C, D, E, F, G, H, I] = new AtAux[A, B, C, D, E, F, G, H, I]

   object build extends Poly9 {
     val functions = self.functions
     implicit def allCases[A, B, C, D, E, F, G, H, I, Out](implicit tL: Function9TypeAt[A, B, C, D, E, F, G, H, I, Out, HL]): Case.Aux[A, B, C, D, E, F, G, H, I, Out] =
       at(tL(functions))
   }
 }

 /* For internal use of Poly9Builder */
 trait Function9TypeAt[A, B, C, D, E, F, G, H, I, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I) => Out
 }

 object Function9TypeAt {
   implicit def at0[A, B, C, D, E, F, G, H, I, Out, Tail <: HList]: Function9TypeAt[A, B, C, D, E, F, G, H, I, Out, ((A, B, C, D, E, F, G, H, I) => Out)::Tail] =
     new Function9TypeAt[A, B, C, D, E, F, G, H, I, Out, ((A, B, C, D, E, F, G, H, I) => Out)::Tail] {
       def apply(l: ((A, B, C, D, E, F, G, H, I) => Out)::Tail): (A, B, C, D, E, F, G, H, I) => Out = l.head
     }

   implicit def atOther[A, B, C, D, E, F, G, H, I, Out, Tail <: HList, Head](implicit tprev: Function9TypeAt[A, B, C, D, E, F, G, H, I, Out, Tail]): Function9TypeAt[A, B, C, D, E, F, G, H, I, Out, Head::Tail] =
     new Function9TypeAt[A, B, C, D, E, F, G, H, I, Out, Head::Tail] {
       def apply(l: Head::Tail): (A, B, C, D, E, F, G, H, I) => Out = tprev(l.tail)
     }
 }

 trait Poly10Builder[HL <: HList] extends PolyApply { self =>
   type λ = build.type
   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J) => Out): Poly10Builder[((A, B, C, D, E, F, G, H, I, J) => Out) :: HL] = {
       new Poly10Builder[((A, B, C, D, E, F, G, H, I, J) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }

   def at[A, B, C, D, E, F, G, H, I, J] = new AtAux[A, B, C, D, E, F, G, H, I, J]

   object build extends Poly10 {
     val functions = self.functions
     implicit def allCases[A, B, C, D, E, F, G, H, I, J, Out](implicit tL: Function10TypeAt[A, B, C, D, E, F, G, H, I, J, Out, HL]): Case.Aux[A, B, C, D, E, F, G, H, I, J, Out] =
       at(tL(functions))
   }
 }

 /* For internal use of Poly10Builder */
 trait Function10TypeAt[A, B, C, D, E, F, G, H, I, J, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J) => Out
 }

 object Function10TypeAt {
   implicit def at0[A, B, C, D, E, F, G, H, I, J, Out, Tail <: HList]: Function10TypeAt[A, B, C, D, E, F, G, H, I, J, Out, ((A, B, C, D, E, F, G, H, I, J) => Out)::Tail] =
     new Function10TypeAt[A, B, C, D, E, F, G, H, I, J, Out, ((A, B, C, D, E, F, G, H, I, J) => Out)::Tail] {
       def apply(l: ((A, B, C, D, E, F, G, H, I, J) => Out)::Tail): (A, B, C, D, E, F, G, H, I, J) => Out = l.head
     }

   implicit def atOther[A, B, C, D, E, F, G, H, I, J, Out, Tail <: HList, Head](implicit tprev: Function10TypeAt[A, B, C, D, E, F, G, H, I, J, Out, Tail]): Function10TypeAt[A, B, C, D, E, F, G, H, I, J, Out, Head::Tail] =
     new Function10TypeAt[A, B, C, D, E, F, G, H, I, J, Out, Head::Tail] {
       def apply(l: Head::Tail): (A, B, C, D, E, F, G, H, I, J) => Out = tprev(l.tail)
     }
 }

 trait Poly11Builder[HL <: HList] extends PolyApply { self =>
   type λ = build.type
   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J, K] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J, K) => Out): Poly11Builder[((A, B, C, D, E, F, G, H, I, J, K) => Out) :: HL] = {
       new Poly11Builder[((A, B, C, D, E, F, G, H, I, J, K) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }

   def at[A, B, C, D, E, F, G, H, I, J, K] = new AtAux[A, B, C, D, E, F, G, H, I, J, K]

   object build extends Poly11 {
     val functions = self.functions
     implicit def allCases[A, B, C, D, E, F, G, H, I, J, K, Out](implicit tL: Function11TypeAt[A, B, C, D, E, F, G, H, I, J, K, Out, HL]): Case.Aux[A, B, C, D, E, F, G, H, I, J, K, Out] =
       at(tL(functions))
   }
 }

 /* For internal use of Poly11Builder */
 trait Function11TypeAt[A, B, C, D, E, F, G, H, I, J, K, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J, K) => Out
 }

 object Function11TypeAt {
   implicit def at0[A, B, C, D, E, F, G, H, I, J, K, Out, Tail <: HList]: Function11TypeAt[A, B, C, D, E, F, G, H, I, J, K, Out, ((A, B, C, D, E, F, G, H, I, J, K) => Out)::Tail] =
     new Function11TypeAt[A, B, C, D, E, F, G, H, I, J, K, Out, ((A, B, C, D, E, F, G, H, I, J, K) => Out)::Tail] {
       def apply(l: ((A, B, C, D, E, F, G, H, I, J, K) => Out)::Tail): (A, B, C, D, E, F, G, H, I, J, K) => Out = l.head
     }

   implicit def atOther[A, B, C, D, E, F, G, H, I, J, K, Out, Tail <: HList, Head](implicit tprev: Function11TypeAt[A, B, C, D, E, F, G, H, I, J, K, Out, Tail]): Function11TypeAt[A, B, C, D, E, F, G, H, I, J, K, Out, Head::Tail] =
     new Function11TypeAt[A, B, C, D, E, F, G, H, I, J, K, Out, Head::Tail] {
       def apply(l: Head::Tail): (A, B, C, D, E, F, G, H, I, J, K) => Out = tprev(l.tail)
     }
 }

 trait Poly12Builder[HL <: HList] extends PolyApply { self =>
   type λ = build.type
   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J, K, L] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J, K, L) => Out): Poly12Builder[((A, B, C, D, E, F, G, H, I, J, K, L) => Out) :: HL] = {
       new Poly12Builder[((A, B, C, D, E, F, G, H, I, J, K, L) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }

   def at[A, B, C, D, E, F, G, H, I, J, K, L] = new AtAux[A, B, C, D, E, F, G, H, I, J, K, L]

   object build extends Poly12 {
     val functions = self.functions
     implicit def allCases[A, B, C, D, E, F, G, H, I, J, K, L, Out](implicit tL: Function12TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, Out, HL]): Case.Aux[A, B, C, D, E, F, G, H, I, J, K, L, Out] =
       at(tL(functions))
   }
 }

 /* For internal use of Poly12Builder */
 trait Function12TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J, K, L) => Out
 }

 object Function12TypeAt {
   implicit def at0[A, B, C, D, E, F, G, H, I, J, K, L, Out, Tail <: HList]: Function12TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, Out, ((A, B, C, D, E, F, G, H, I, J, K, L) => Out)::Tail] =
     new Function12TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, Out, ((A, B, C, D, E, F, G, H, I, J, K, L) => Out)::Tail] {
       def apply(l: ((A, B, C, D, E, F, G, H, I, J, K, L) => Out)::Tail): (A, B, C, D, E, F, G, H, I, J, K, L) => Out = l.head
     }

   implicit def atOther[A, B, C, D, E, F, G, H, I, J, K, L, Out, Tail <: HList, Head](implicit tprev: Function12TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, Out, Tail]): Function12TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, Out, Head::Tail] =
     new Function12TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, Out, Head::Tail] {
       def apply(l: Head::Tail): (A, B, C, D, E, F, G, H, I, J, K, L) => Out = tprev(l.tail)
     }
 }

 trait Poly13Builder[HL <: HList] extends PolyApply { self =>
   type λ = build.type
   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J, K, L, M) => Out): Poly13Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M) => Out) :: HL] = {
       new Poly13Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }

   def at[A, B, C, D, E, F, G, H, I, J, K, L, M] = new AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M]

   object build extends Poly13 {
     val functions = self.functions
     implicit def allCases[A, B, C, D, E, F, G, H, I, J, K, L, M, Out](implicit tL: Function13TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, Out, HL]): Case.Aux[A, B, C, D, E, F, G, H, I, J, K, L, M, Out] =
       at(tL(functions))
   }
 }

 /* For internal use of Poly13Builder */
 trait Function13TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J, K, L, M) => Out
 }

 object Function13TypeAt {
   implicit def at0[A, B, C, D, E, F, G, H, I, J, K, L, M, Out, Tail <: HList]: Function13TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M) => Out)::Tail] =
     new Function13TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M) => Out)::Tail] {
       def apply(l: ((A, B, C, D, E, F, G, H, I, J, K, L, M) => Out)::Tail): (A, B, C, D, E, F, G, H, I, J, K, L, M) => Out = l.head
     }

   implicit def atOther[A, B, C, D, E, F, G, H, I, J, K, L, M, Out, Tail <: HList, Head](implicit tprev: Function13TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, Out, Tail]): Function13TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, Out, Head::Tail] =
     new Function13TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, Out, Head::Tail] {
       def apply(l: Head::Tail): (A, B, C, D, E, F, G, H, I, J, K, L, M) => Out = tprev(l.tail)
     }
 }

 trait Poly14Builder[HL <: HList] extends PolyApply { self =>
   type λ = build.type
   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Out): Poly14Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Out) :: HL] = {
       new Poly14Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }

   def at[A, B, C, D, E, F, G, H, I, J, K, L, M, N] = new AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N]

   object build extends Poly14 {
     val functions = self.functions
     implicit def allCases[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out](implicit tL: Function14TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out, HL]): Case.Aux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out] =
       at(tL(functions))
   }
 }

 /* For internal use of Poly14Builder */
 trait Function14TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Out
 }

 object Function14TypeAt {
   implicit def at0[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out, Tail <: HList]: Function14TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Out)::Tail] =
     new Function14TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Out)::Tail] {
       def apply(l: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Out)::Tail): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Out = l.head
     }

   implicit def atOther[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out, Tail <: HList, Head](implicit tprev: Function14TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out, Tail]): Function14TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out, Head::Tail] =
     new Function14TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Out, Head::Tail] {
       def apply(l: Head::Tail): (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Out = tprev(l.tail)
     }
 }

 trait Poly15Builder[HL <: HList] extends PolyApply { self =>
   type λ = build.type
   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Out): Poly15Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Out) :: HL] = {
       new Poly15Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }

   def at[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O] = new AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]

   object build extends Poly15 {
     val functions = self.functions
     implicit def allCases[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out](implicit tL: Function15TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out, HL]): Case.Aux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out] =
       at(tL(functions))
   }
 }

 /* For internal use of Poly15Builder */
 trait Function15TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Out
 }

 object Function15TypeAt {
   implicit def at0[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out, Tail <: HList]: Function15TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Out)::Tail] =
     new Function15TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Out)::Tail] {
       def apply(l: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Out)::Tail): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Out = l.head
     }

   implicit def atOther[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out, Tail <: HList, Head](implicit tprev: Function15TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out, Tail]): Function15TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out, Head::Tail] =
     new Function15TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Out, Head::Tail] {
       def apply(l: Head::Tail): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Out = tprev(l.tail)
     }
 }

 trait Poly16Builder[HL <: HList] extends PolyApply { self =>
   type λ = build.type
   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Out): Poly16Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Out) :: HL] = {
       new Poly16Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }

   def at[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] = new AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]

   object build extends Poly16 {
     val functions = self.functions
     implicit def allCases[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out](implicit tL: Function16TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out, HL]): Case.Aux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out] =
       at(tL(functions))
   }
 }

 /* For internal use of Poly16Builder */
 trait Function16TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Out
 }

 object Function16TypeAt {
   implicit def at0[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out, Tail <: HList]: Function16TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Out)::Tail] =
     new Function16TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Out)::Tail] {
       def apply(l: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Out)::Tail): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Out = l.head
     }

   implicit def atOther[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out, Tail <: HList, Head](implicit tprev: Function16TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out, Tail]): Function16TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out, Head::Tail] =
     new Function16TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Out, Head::Tail] {
       def apply(l: Head::Tail): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Out = tprev(l.tail)
     }
 }

 trait Poly17Builder[HL <: HList] extends PolyApply { self =>
   type λ = build.type
   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Out): Poly17Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Out) :: HL] = {
       new Poly17Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }

   def at[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q] = new AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]

   object build extends Poly17 {
     val functions = self.functions
     implicit def allCases[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out](implicit tL: Function17TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out, HL]): Case.Aux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out] =
       at(tL(functions))
   }
 }

 /* For internal use of Poly17Builder */
 trait Function17TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Out
 }

 object Function17TypeAt {
   implicit def at0[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out, Tail <: HList]: Function17TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Out)::Tail] =
     new Function17TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Out)::Tail] {
       def apply(l: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Out)::Tail): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Out = l.head
     }

   implicit def atOther[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out, Tail <: HList, Head](implicit tprev: Function17TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out, Tail]): Function17TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out, Head::Tail] =
     new Function17TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Out, Head::Tail] {
       def apply(l: Head::Tail): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Out = tprev(l.tail)
     }
 }

 trait Poly18Builder[HL <: HList] extends PolyApply { self =>
   type λ = build.type
   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Out): Poly18Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Out) :: HL] = {
       new Poly18Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }

   def at[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R] = new AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]

   object build extends Poly18 {
     val functions = self.functions
     implicit def allCases[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out](implicit tL: Function18TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out, HL]): Case.Aux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out] =
       at(tL(functions))
   }
 }

 /* For internal use of Poly18Builder */
 trait Function18TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Out
 }

 object Function18TypeAt {
   implicit def at0[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out, Tail <: HList]: Function18TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Out)::Tail] =
     new Function18TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Out)::Tail] {
       def apply(l: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Out)::Tail): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Out = l.head
     }

   implicit def atOther[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out, Tail <: HList, Head](implicit tprev: Function18TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out, Tail]): Function18TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out, Head::Tail] =
     new Function18TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Out, Head::Tail] {
       def apply(l: Head::Tail): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Out = tprev(l.tail)
     }
 }

 trait Poly19Builder[HL <: HList] extends PolyApply { self =>
   type λ = build.type
   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Out): Poly19Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Out) :: HL] = {
       new Poly19Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }

   def at[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S] = new AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]

   object build extends Poly19 {
     val functions = self.functions
     implicit def allCases[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out](implicit tL: Function19TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out, HL]): Case.Aux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out] =
       at(tL(functions))
   }
 }

 /* For internal use of Poly19Builder */
 trait Function19TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Out
 }

 object Function19TypeAt {
   implicit def at0[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out, Tail <: HList]: Function19TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Out)::Tail] =
     new Function19TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Out)::Tail] {
       def apply(l: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Out)::Tail): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Out = l.head
     }

   implicit def atOther[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out, Tail <: HList, Head](implicit tprev: Function19TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out, Tail]): Function19TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out, Head::Tail] =
     new Function19TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Out, Head::Tail] {
       def apply(l: Head::Tail): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Out = tprev(l.tail)
     }
 }

 trait Poly20Builder[HL <: HList] extends PolyApply { self =>
   type λ = build.type
   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Out): Poly20Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Out) :: HL] = {
       new Poly20Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }

   def at[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T] = new AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]

   object build extends Poly20 {
     val functions = self.functions
     implicit def allCases[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out](implicit tL: Function20TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out, HL]): Case.Aux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out] =
       at(tL(functions))
   }
 }

 /* For internal use of Poly20Builder */
 trait Function20TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Out
 }

 object Function20TypeAt {
   implicit def at0[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out, Tail <: HList]: Function20TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Out)::Tail] =
     new Function20TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Out)::Tail] {
       def apply(l: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Out)::Tail): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Out = l.head
     }

   implicit def atOther[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out, Tail <: HList, Head](implicit tprev: Function20TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out, Tail]): Function20TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out, Head::Tail] =
     new Function20TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Out, Head::Tail] {
       def apply(l: Head::Tail): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Out = tprev(l.tail)
     }
 }

 trait Poly21Builder[HL <: HList] extends PolyApply { self =>
   type λ = build.type
   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Out): Poly21Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Out) :: HL] = {
       new Poly21Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }

   def at[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U] = new AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]

   object build extends Poly21 {
     val functions = self.functions
     implicit def allCases[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out](implicit tL: Function21TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out, HL]): Case.Aux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out] =
       at(tL(functions))
   }
 }

 /* For internal use of Poly21Builder */
 trait Function21TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Out
 }

 object Function21TypeAt {
   implicit def at0[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out, Tail <: HList]: Function21TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Out)::Tail] =
     new Function21TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Out)::Tail] {
       def apply(l: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Out)::Tail): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Out = l.head
     }

   implicit def atOther[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out, Tail <: HList, Head](implicit tprev: Function21TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out, Tail]): Function21TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out, Head::Tail] =
     new Function21TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Out, Head::Tail] {
       def apply(l: Head::Tail): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Out = tprev(l.tail)
     }
 }

 trait Poly22Builder[HL <: HList] extends PolyApply { self =>
   type λ = build.type
   val functions: HL
   class AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] {
     def apply[Out](λ: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Out): Poly22Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Out) :: HL] = {
       new Poly22Builder[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Out) :: HL] {
         val functions = λ :: self.functions
       }
     }
   }

   def at[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V] = new AtAux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]

   object build extends Poly22 {
     val functions = self.functions
     implicit def allCases[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out](implicit tL: Function22TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out, HL]): Case.Aux[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out] =
       at(tL(functions))
   }
 }

 /* For internal use of Poly22Builder */
 trait Function22TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out, HL <: HList] {
   def apply(l: HL): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Out
 }

 object Function22TypeAt {
   implicit def at0[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out, Tail <: HList]: Function22TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Out)::Tail] =
     new Function22TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out, ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Out)::Tail] {
       def apply(l: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Out)::Tail): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Out = l.head
     }

   implicit def atOther[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out, Tail <: HList, Head](implicit tprev: Function22TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out, Tail]): Function22TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out, Head::Tail] =
     new Function22TypeAt[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Out, Head::Tail] {
       def apply(l: Head::Tail): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Out = tprev(l.tail)
     }
 }
}
