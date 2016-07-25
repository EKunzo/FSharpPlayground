#nowarn "40"

namespace EulerProblems

open System.Collections.Generic;

module EulerProblems = 

    //Simple memoization
    let memoize f = 
        let cache = new Dictionary<_, _>()
        (fun x -> match cache.TryGetValue(x) with
                  | true, y -> y
                  | _ -> let v = f(x)
                         cache.Add(x, v)
                         v
        )

    //check if number is prime
    let isPrime n = 
            match n with
            | 0L -> false
            | 1L -> true
            | _  -> let d = seq { for i in 1L .. n do if n % i = 0L then yield i }
                    (d |> Seq.truncate 2 |> Seq.item 1) = n


    let MultiplesOf3And5 n = 
        seq { for i in 0 .. (n - 1) do
                if(i % 3 = 0 || i % 5 = 0) then yield i } |> Seq.sum
    
    
        
    let EvenFibonacciNumbers limit =
        let rec fibonacci = 
            memoize (fun n -> 
                            match n with 
                            | 0 -> 0
                            | 1 | 2 -> 1
                            | _ -> fibonacci (n - 1) + fibonacci (n - 2)
            ) 

        let fibSequence = Seq.initInfinite(fun index -> fibonacci index)
        let limitIndex = fibSequence |> Seq.tryFindIndex (fun x -> x > limit)

        match limitIndex with 
        | None -> failwith("Wrong limit chosen")
        | Some x -> fibSequence 
                        |> Seq.truncate x
                        |> Seq.sumBy (fun a -> if (a % 2 = 0) then a else 0)



    let LargestPrimeFactor number = 
        let possibleFactors = seq { for i in 1L .. (number/2L) do yield i }
        let result = possibleFactors 
                     |> Seq.filter(fun x -> isPrime x)
                     |> Seq.sortBy(fun x -> x)
                     |> Seq.last

        result



    let LargestPalindromeNumber digits = 
        let checkIfPalindrome (number:int) = 
            let stringRepr = string number
            let chars = [ for c in stringRepr do yield c ]
            let revertedChars = chars |> List.rev
            chars = revertedChars
        let max = 10.0 ** digits - 1.0
        let min = 10.0 ** (digits - 1.0)

        [ for i in min .. max do 
                for j in i .. max do
                yield if checkIfPalindrome (int (i * j)) then (i * j) else 0.0
        ] 
        |> List.filter (fun x -> not (x = 0.0))
        |> List.max



    let xThPrime x = 
        
        let primes = Seq.initInfinite (fun n -> match n with
                                                | 0 -> (0, false)
                                                | 1 -> (1, false)
                                                | _ ->(n, isPrime (int64(n))) )
        fst (primes |> Seq.filter (fun (_, y) -> y = true) |> Seq.item (x - 1)) //because index starts at 0, and user usually starts at 1



    let SmallestMultiple n = 
        let checkDivisorArray divisors n =
            let result = seq { for d in divisors -> n % d = 0 }
            result |> Seq.forall (fun x -> x = true)

        let array = [for i in 1 .. n -> i ]
        let result = Seq.initInfinite (fun x -> match x with 
                                                | 0 -> (0, false)
                                                | _ -> (x, checkDivisorArray array x))

        result |> Seq.find (fun (_, y) -> y = true)
        


    let SumSquareDifference n = 
        let power (i:int) = (float i) ** 2.0
        let array = [|for i in 1 .. n -> i |]
        let sumSquares = array |> Array.map (fun i -> (power i)) |> Array.sum
        let squareSum = 
            let i = array |> Array.sum; 
            power i

        squareSum - sumSquares


    let LargestProductInSeries digits =
        let series = 
            "73167176531330624919225119674426574742355349194934" +
            "96983520312774506326239578318016984801869478851843" +
            "85861560789112949495459501737958331952853208805511" +
            "12540698747158523863050715693290963295227443043557" +
            "66896648950445244523161731856403098711121722383113" +
            "62229893423380308135336276614282806444486645238749" +
            "30358907296290491560440772390713810515859307960866" +
            "70172427121883998797908792274921901699720888093776" +
            "65727333001053367881220235421809751254540594752243" +
            "52584907711670556013604839586446706324415722155397" +
            "53697817977846174064955149290862569321978468622482" +
            "83972241375657056057490261407972968652414535100474" +
            "82166370484403199890008895243450658541227588666881" +
            "16427171479924442928230863465674813919123162824586" +
            "17866458359124566529476545682848912883142607690042" +
            "24219022671055626321111109370544217506941658960408" +
            "07198403850962455444362981230987879927244284909188" +
            "84580156166097919133875499200524063689912560717606" +
            "05886116467109405077541002256983155200055935729725" +
            "71636269561882670428252483600823257530420752963450"

        let s = series 
                |> Seq.map (fun x -> System.Int64.Parse(x.ToString()))
                |> Seq.toArray
        let rec getDigits maximum ser =
            if (ser |> Seq.length) < digits then 
                maximum
            else 
                let currentSeries = ser |> Seq.truncate digits 
                let currentProduct = currentSeries |> Seq.fold (fun acc elem -> acc * elem) 1L
                let newMaximum = if (currentProduct > maximum) then currentProduct else maximum
                getDigits newMaximum (ser |> Seq.tail)

        getDigits 0L s