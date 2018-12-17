module Main (main) where

import Test.DocTest (doctest)


main :: IO ()
main = do
    doctest ["Day 01 - Chronal Calibration/Main.hs"]
    doctest ["Day 02 - Inventory Management System/Main.hs"]
    doctest ["Day 03 - No Matter How You Slice It/Main.hs"]
