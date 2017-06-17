module Ch10WarmUp where
-- 1.
stops = "pbtdkg"
vowels = "aeiou"

warmupA = [(s1, v, x2) | s1 <- stops, v <- vowels, x2 <- stops]
warmupB = [(s1, v, x2) | s1 <- stops, v <- vowels, x2 <- stops, s1 == 'p']

verbs = ["code", "eat", "sleep", "swim", "bike"]
nouns = ["notebook", "pen", "laptop", "mug", "coffee", "bike"]

warmupC = [(n1, v, n2) | n1 <- nouns, v <- verbs, n2 <- nouns]

-- 2.
{--
This function splits the String into String[] at the ' ' charcter, then
each word gets mapped to a its length; then these numbers get summed, after
that the sum is divided by the number of words in the given string. Thanks to
that we gen an average length of the word.
The type is seekritFunc :: String -> Integral
--}

--seekritFunc :: String ->
seekritFunc x =
  (/) (realToFrac (sum (map length (words x))))
      (realToFrac (length (words x)))
