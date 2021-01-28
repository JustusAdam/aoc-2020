#!/usr/bin/env stack
-- stack --resolver lts-16.15 script --package containers --package vector

{-# LANGUAGE TupleSections, LambdaCase, FlexibleInstances, ViewPatterns #-}

import Data.List (find, span, tails, sort)
import Data.Maybe (fromJust, mapMaybe)
import System.Environment
import Control.Arrow (second, first, (***) )
import Debug.Trace
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Char
import Control.Monad
import Control.Applicative ((<|>))
import Control.Monad.ST.Strict
import Data.STRef.Strict
import Data.Function
import qualified Data.IntMap as IM
import Control.Category ((>>>))
import GHC.Arr hiding (indices)
import Data.Foldable
import Data.Tuple

type State = ((Int, Int), Char)

solve :: [String] -> Int
solve = uncurry ((+) `on` abs) . fst . foldr' f ((0, 0), 'E') . reverse
  where
    dirMap = \case
        'N' -> first . (+)
        'S' -> first . flip (-)
        'E' -> second . (+)
        'W' -> second . flip (-)
    rotMap = "NESW"
    f (cmd:(read -> arg)) =
        case cmd of
            'F' -> \(loc, dir) -> (dirMap dir arg loc, dir)
            'R' -> second $ (!!(arg `div` 90)) . flip dropWhile (cycle rotMap) . (/=)
            'L' -> second $ (!!(arg `div` 90)) . flip dropWhile (cycle $ reverse rotMap) . (/=)
            other -> first $ dirMap other arg

type State2 = ((Int, Int), (Int, Int))

both f = f *** f

solve2 :: [String] -> Int
solve2 = uncurry ((+) `on` abs) . snd . foldr' f ((1, 10), (0,0)) . reverse
  where
    dirMap = \case
        'N' -> first . (+)
        'S' -> first . flip (-)
        'E' -> second . (+)
        'W' -> second . flip (-)
    rotMap = [swap . (id *** negate), (negate *** negate), swap . (negate *** id)]
    f inp@(cmd:(read -> arg)) =
        case cmd of
            'F' -> \(wp@(wx, wy), (accx, accy)) -> (wp, (wx * arg + accx, wy * arg + accy))
            'R' -> first $ (cycle rotMap !!(pred $ arg `div` 90))
            'L' -> first $ (cycle (reverse rotMap) !! (pred $ arg `div` 90))
            other -> first $ dirMap other arg

main = do
    inp <- lines <$> getContents
    print $ solve2 inp
