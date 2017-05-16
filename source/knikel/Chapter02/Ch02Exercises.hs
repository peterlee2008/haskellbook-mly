{-- Parenthesization
1. 2 + 2 * 3 - 1
>> 2 + (2 * 3) -1
2. (^) 10 $ 1 + 1
>> (^) 10 (1 + 1)
3. 2 ^ 2 * 4 ^ 5 + 1
>> (2 ^ 2) * (4 ^ 5) + 1
--}


{-- Equivalent expressions

1. Yes
2. Yes -> 10 + (9 * 10)
3. No -> 363 and -363
4. No -> 33 and 33.3333...
5. No -> 28 and 46

--}

{-- More fun with functions

let z = 7
let y = z + 8
let x = y ^ 2
let waxOn = x * 5

--}

module Ch02Exercises where

waxOn = x * 5 where
  x = y ^ 2
  y = z + 8
  z = 7

triple x = x * 3

waxOff x = triple x
