module Main where

import Regexp
import IOManager

type Test = ((String, String), [String])

main :: IO()
main = wrapIO solve

solve :: Input -> Output -> Output
solve input output = writeStdOut output result
  where
    result = formatted ss ++ "\n\n" ++ score ss
    ss     = statuses ts
    ts     = tests $ getInputFile input "tests.txt"

statuses :: [Test] -> [String]
statuses tests = map status tests
  where
    status test = if generatedOutput == expectedOutput
                  then "OK"
                  else "FAILED"
                      ++ " input "    ++ show input
                      ++ " got "      ++ show generatedOutput
                      ++ " expected " ++ show expectedOutput
      where
        generatedOutput         = uncurry matches input
        (input, expectedOutput) = test

tests :: String -> [Test]
tests contents = readLines contents :: [Test]

formatted :: [String] -> String
formatted ss = concat $
    zipWith (++) (map (("\n" ++) . (++ " ") . show) [1 .. length ss]) ss

score :: [String] -> String
score ss = "Punctaj: " ++ (show passed)
  where
    passed = length $ filter ((== 'O') . head) ss

-- | Breaks the string into lines and reads each one, individually.
readLines :: (Read a) => String -> [a]
readLines = map read . lines