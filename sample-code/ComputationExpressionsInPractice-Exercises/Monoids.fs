﻿namespace ComputationExpressionsInPractice
open FsCheck

module MonoidSetup =
  type Colour = 
      { r : byte
        g : byte
        b : byte
        a : byte }

  let addColour c1 c2 = 
      { r = c1.r + c2.r
        g = c1.g + c2.g
        b = c1.b + c2.b
        a = c1.a + c2.a }

  let neutral = { 
        r = 0uy
        g = 0uy
        b = 0uy
        a = 0uy }

  let c1 = { neutral with g = 254uy }
  let c2 = { neutral with r = 254uy }
  
  
  

module AddingTwoColours =
  open MonoidSetup

  let addColours (colours : Colour list) = 
      let mutable res = 
          { r = 0uy
            g = 0uy
            b = 0uy
            a = 0uy }
      for i in colours do
          res <- addColour res i
      res

  type Monoid<'a> = 
      { neutral : 'a
        op : 'a -> 'a -> 'a }


  let l = [ c1; c2; neutral ] |> List.reduce (addColour)

  let (+++) = addColour
  let x = c1 +++ c2 +++ { c2 with a = 254uy }

  type TColour = Colour
  let colourAdd : Monoid<Colour> = 
    { neutral = neutral
      op = (addColour) }

  let M = colourAdd
  let Z = M.neutral
  let (++) = M.op

  let `` Z is the neutral element`` (v : TColour) = 
    Z ++ v = v && v ++ Z = v

  let ``The operation is commutative`` (a : TColour, b : TColour, c : TColour) = 
    a ++ (b ++ c) = (a ++ b ++ c)

  Check.Quick `` Z is the neutral element`` 
  Check.Quick ``The operation is commutative``
module UnderstandingZero =
(*
6. Implement a builder with a Zero member, maske sure you 
   print something from within it.
*)
    //type ZeroBuilder ...

//    let zero = ZeroBuilder()       
//    let justPrint ()= zero {
//        printfn "Inside the computation expression"
//    }
//    justPrint()
//    
//    let withIf (x:bool) = zero {
//       if (x) then
//           printfn("This is true")    
//    }
//    withIf false


module MonoidsWithComputationExpressions = 
  open MonoidSetup
(*
7. Implement a builder for a monoid. 
   Hint: You will need Zero and Combine , 
    For and Yield might be a good idea too. 
    I replaced all methods with c1 so that this will compile
*)
  type MonoidBuilder ()= 
    member this.Zero() = 
        c1

    member this.Combine(x, y) = 
        c1
    
    member x.For(sequence, f) =
        c1

    member x.Yield (a) = c1


  let monoid = new MonoidBuilder()

  let monoidAdd xs= monoid {
       for x in xs do
         yield x
       }
  
  let ``Adding colours in reverse result in the same colour``(xs : Colour list) = 
    let sxs = List.ofSeq xs
    monoidAdd sxs = monoidAdd (List.rev sxs)
  
  Check.Quick ``Adding colours in reverse result in the same colour``

