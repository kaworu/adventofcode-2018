module Main (main) where

import Test.DocTest (doctest)

main :: IO ()
main = do
    doctest ["Day 01 - Chronal Calibration/Main.hs"]
    doctest ["Day 02 - Inventory Management System/Main.hs"]
    doctest ["Day 03 - No Matter How You Slice It/Main.hs"]
    doctest ["Day 04 - Repose Record/Main.hs"]
    doctest ["Day 05 - Alchemical Reduction/Main.hs"]
    doctest ["Day 06 - Chronal Coordinates/Main.hs"]
    doctest ["Day 07 - The Sum of Its Parts/Main.hs"]
    doctest ["Day 08 - Memory Maneuver/Main.hs"]
    doctest ["Day 09 - Marble Mania/Main.hs"]
    doctest ["Day 10 - The Stars Align/Main.hs"]
    doctest ["Day 11 - Chronal Charge/Main.hs"]
    doctest ["Day 12 - Subterranean Sustainability/Main.hs"]
