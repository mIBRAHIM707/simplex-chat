module Main where

import Test.Hspec (hspec)
import ReadReceiptsTests (readReceiptsTests)

main :: IO ()
main = hspec readReceiptsTests
