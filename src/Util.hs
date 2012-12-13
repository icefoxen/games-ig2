{- Handy things that don't fit elsewhere.
-}

module Util where
import Control.Monad
import Data.Ratio
import Random
import System.Directory
import System.FilePath


-- Turn a probability distribution function into a cumulative distribution
-- function.  So we have [(a, 0.1), (b, 0.5), (c, 0.4)] and it turns into
-- [(a, 0.1), (b, 0.6), (c, 1.0)] so that we can easily select from it.
-- XXX: Note that this does not check whether the values in the PDF actually
-- add up to 1.0.
-- I can think of a few ways of fixing this, all of them bad.
-- Right now if you put in a value that falls off the end of your 
-- CDF then it just chooses the last one, and if you put in a value
-- smaller than the max value of your CDF then it just never picks
-- things bigger than it.
pdf2cdf :: [(a, Double)] -> [(a, Double)]
pdf2cdf pdf =
  pdf2cdfHelper pdf 0 []

pdf2cdfHelper :: [(a, Double)] -> Double -> [(a, Double)] -> [(a, Double)]
pdf2cdfHelper [] current accm = reverse accm
pdf2cdfHelper (fst:pdf) current accm =
  let (itm,prob) = fst
      next = current + prob
      in
   pdf2cdfHelper pdf next $ (itm, next) : accm

selectFromCDF :: Monad m => Double -> [(b, Double)] -> m b
selectFromCDF val [] = fail "selectFromCDF: Fell off the end of the list."
selectFromCDF val (hd:[]) = do
  let (itm, prob) = hd
  return itm
selectFromCDF val (hd:tl) = do
  let (itm,prob) = hd
  if val > prob then (selectFromCDF val tl)
    else return itm


randomFromList :: [a] -> IO a
randomFromList lst = do
  let len = length lst
  n <- randomRIO (1, (len - 1))
  return (head (drop n lst))

-- Produces a list of random numbers no larger than b, that add up to a
splitInto :: Integer -> Integer -> IO [Integer]
splitInto a b =
  splitIntoHelper a b []

splitIntoHelper :: Integer -> Integer -> [Integer] -> IO [Integer]
splitIntoHelper a b accm =
  if a < b then do return (a : accm)
    else do
    r <- randomRIO (1, b)
    splitIntoHelper (a - r) b (r : accm)


-- These might not be the right way of handling it.
rationalToInteger :: Rational -> Integer
rationalToInteger r = numerator (approxRational r 0.5)

rationalToDouble :: Rational -> Double
rationalToDouble r = fromRational r


-- XXX: The getDirectoryContents operation can fail
-- in various and sundry horrifying ways that should
-- be dealt with nicely.
-- Oh gods making things simpler makes them more complex.
-- Gotta filter directory names such as . and .. out of 
-- the list of files.
getFilesInDir :: FilePath -> IO [String]
getFilesInDir path = do
  dircontents <- getDirectoryContents path
  filterM (\f -> System.Directory.doesFileExist $ joinPath [path, f]) 
    dircontents

-- Take a list and return an infinite list
-- of random items from it.
-- Efficiency = ???
makeRandomList :: [a] -> StdGen -> [a]
makeRandomList lst gen =
    -- generate an infinite list of random numbers
    -- and now use them to generate an infinite list of strings 
    -- print them out
    let ns = randomRs (0,length lst-1) gen in
    map (lst !!) ns
    
