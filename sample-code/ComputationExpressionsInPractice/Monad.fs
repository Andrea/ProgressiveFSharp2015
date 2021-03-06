﻿namespace ComputationExpressionsInPractice
    
type MaybeBuilder() = 
(*
3. For this step you will need to implement Bind for the Maybe builder
*)
      member __.Bind(maybeValue: 'a option, func) : 'b option= 
          match maybeValue with
          | Some value -> func value
          | None -> None
        
      member __.Return value = Some value
      member this.ReturnFrom value = this.Bind(value, this.Return)
    

module ``Divide by zero `` =
    // This is what we used to do but not a good idea
    let division a b c d = 
        match b with
        | 0 -> None
        | _ -> 
            match c with
            | 0 -> None
            | _ -> 
                match d with
                | 0 -> None
                | _ -> Some(((a / b) / c) / d)
    
    let divide a b = 
        match b with
        | 0 -> None
        | _ -> Some(a / b)
    let maybe = MaybeBuilder()


    let divisionMCE a b c d = 
      maybe { let! x = divide a b
              let! y = divide x c
              let! z = divide y d
              return z }


    let divisionM a b c d = 
      maybe.Bind(divide a b, fun x ->
        maybe.Bind(divide x c, fun y ->
          maybe.Bind( divide y d, fun z ->
            maybe.Return(z))))

    do divisionM 120 4 3 2 = divisionMCE 120 4 3 2 |> printfn "Should be true: %A"
    
(*
4. Change divisionMCE2 so that it returns an int option
*)

    let divisionMCE2 a b c d : int option= 
      maybe { let! x = divide a b
              let! y = divide x c
              let! z = divide y d
              return! Some z }