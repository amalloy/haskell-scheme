module Flatland.Scheme.Main (main) where

import Flatland.Scheme.Reader
import System.IO

main :: IO ()
main = do
  program <- getContents
  print (readEval program)
