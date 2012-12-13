{- Entry point, setup, and such.
-}

module Main where
import Control.Monad.State

import Loader
import Universe
import UniverseGen

{-
runTurn :: Galaxy -> Galaxy
runTurn g =
  g
-}

main :: IO ()
main = do
  s <- standardUniverseSeed
  galaxy <- genGalaxy s
  putStrLn $ show $ galaxy