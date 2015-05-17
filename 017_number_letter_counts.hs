-- If the numbers 1 to 5 are written out in words: one, two, three, four, five, then 
-- there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
-- 
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in 
-- words, how many letters would be used?
-- 
-- 
-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two)
-- contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. 
-- The use of "and" when writing out numbers is in compliance with British usage.


-- Idea:
--  -> Number to List
--  -> concatenate last two entries 
--  -> if length == 3 = digitToWord ++ " tousand" ++ restOfList....
--  -> foldr

-- returns the length of the given number when written
digitToWordCount :: Int -> Int
digitToWordCount n
  | n == 0 = 0       --is not spoken within big numbers
  | n == 1 = 3       --one
  | n == 2 = 3       --two
  | n == 3 = 5       --three
  | n == 4 = 4       --four
  | n == 5 = 4       --five
  | n == 6 = 3       --six
  | n == 7 = 5       --seven
  | n == 8 = 5       --eight
  | n == 9 = 4       --nine
  | n == 10 = 3      --ten
  | n == 11 = 6      --eleven
  | n == 12 = 6      --twelve
  | n == 13 = 8      --thirteen
  | n == 15 = 7      --fifteen
  | n == 18 = 8      --eighteen
  | div n 10 == 1 = 4 + digitToWordCount ( mod n 10)  --teen + rest
  | div n 10 == 2 = 6 + digitToWordCount ( mod n 10)  --twenty + rest
  | div n 10 == 3 = 6 + digitToWordCount ( mod n 10)  --thirty + rest
  | div n 10 == 4 = 5 + digitToWordCount ( mod n 10)  --forty + rest
  | div n 10 == 5 = 5 + digitToWordCount ( mod n 10)  --fifty + rest
  | div n 10 == 6 = 5 + digitToWordCount ( mod n 10)  --sixty + rest
  | div n 10 == 7 = 7 + digitToWordCount ( mod n 10)  --seventy + rest
  | div n 10 == 8 = 6 + digitToWordCount ( mod n 10)  --eighty + rest
  | div n 10 == 9 = 6 + digitToWordCount ( mod n 10)  --ninety + rest
  | otherwise = undefined

-- returns a String representation of the given number
digitToWord :: Int -> String
digitToWord n
  | n == 0 = ""             --is not spoken within big numbers
  | n == 1 = "one"          --3
  | n == 2 = "two"          --3
  | n == 3 = "three"        --5
  | n == 4 = "four"         --4
  | n == 5 = "five"         --4
  | n == 6 = "six"          --3
  | n == 7 = "seven"        --5
  | n == 8 = "eight"        --5
  | n == 9 = "nine"         --4
  | n == 10 = "ten"         --3
  | n == 11 = "eleven"      --6
  | n == 12 = "twelve"      --6
  | n == 13 = "thirteen"    --8
  | n == 15 = "fifteen"     --7
  | n == 18 = "eighteen"    --8
  | div n 10 == 1 = digitToWord ( mod n 10) ++ "teen"
  | div n 10 == 2 = "twenty-" ++ digitToWord ( mod n 10)
  | div n 10 == 3 = "thirty-" ++ digitToWord ( mod n 10) 
  | div n 10 == 4 = "forty-" ++ digitToWord ( mod n 10)
  | div n 10 == 5 = "fifty-" ++ digitToWord ( mod n 10) 
  | div n 10 == 6 = "sixty-" ++ digitToWord ( mod n 10) 
  | div n 10 == 7 = "seventy-" ++ digitToWord ( mod n 10)
  | div n 10 == 8 = "eighty-" ++ digitToWord ( mod n 10)
  | div n 10 == 9 = "ninety-" ++ digitToWord ( mod n 10)
  | otherwise = undefined
  
-- Takes a number and returns the written number as String. Defined from 0 to 9999
numberToWord :: Int -> String
numberToWord n
  | n == 0 = "zero"
  | otherwise = numberToWordAux $ digitsToList2 n
 
numberToWordAux :: [Int] -> String
numberToWordAux (0:x:[]) = digitToWord x
numberToWordAux (x:0:[]) = digitToWord x ++ " hundred"
numberToWordAux (x:y:[]) = digitToWord x ++ " hundred and " ++ digitToWord y
numberToWordAux (x:0:0:[]) = digitToWord x ++ " thousand"
numberToWordAux (x:y:z:[]) = digitToWord x ++ " thousand " ++ numberToWordAux (y:z:[])

-- Takes a number and returns the length of the written number. Defined from 0 to 9999
numberToWordLength :: Int -> Int
numberToWordLength n
  | n == 0 = 4  --zero
  | otherwise = numberToWordLengthAux $ digitsToList2 n

numberToWordLengthAux :: [Int] -> Int
numberToWordLengthAux (0:x:[]) = digitToWordCount x
numberToWordLengthAux (x:0:[]) = digitToWordCount x + 7
numberToWordLengthAux (x:y:[]) = digitToWordCount x + 10 + digitToWordCount y
numberToWordLengthAux (x:0:0:[]) = digitToWordCount x + 8
numberToWordLengthAux (x:y:z:[]) = digitToWordCount x + 8 + numberToWordLengthAux (y:z:[])


-- converts an Integral to a list of digits, the last two digits are one list element
digitsToList2 :: (Integral a) => a -> [a]
digitsToList2 n = digitsToList2Aux (div n 100) [mod n 100]

digitsToList2Aux :: (Integral a) => a -> [a] -> [a]
digitsToList2Aux n nList
  | n < 10 = n : nList
  | otherwise = digitsToList2Aux (div n 10) ((mod n 10) : nList)

main :: IO()
main = putStrLn $ show $ sum $ map numberToWordLength [1..1000]
-- main = putStrLn $ show $ map numberToWord [1..1000]