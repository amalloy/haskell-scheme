module Main (main) where

import Flatland.Scheme.Reader
import System.IO

main :: IO ()
main = interact (show . readEval)
